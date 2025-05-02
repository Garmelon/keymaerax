/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.core.{Formula, Number, Program, Term, Variable}
import org.keymaerax.hippocore.definitions.Definitions
import org.keymaerax.hippocore.proof.{HippoExpression, HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{ExprPath, SequentPrinter}
import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic, PureTactic}
import org.keymaerax.hippolang.HlangConversions.*
import org.keymaerax.hippolang.interpret.InterpreterPure.{
  getSingleArg,
  getValueAsExpression,
  getValueAsInt,
  getValueAsList,
  getValueAsProof,
  getValueAsSequent,
  interpolateExpression,
  interpolateSequent,
}
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.parse.SourceFile
import org.keymaerax.hippolang.{
  BuiltinFunction,
  BuiltinMemberFunction,
  HlangException,
  HlangExpression,
  HlangIdentifier,
  HlangValue,
  PrettyPrinter,
}
import org.keymaerax.hippolib.meta.TacticInfo
import org.keymaerax.hippolib.primitive.Cached

class InterpreterPure(ictx: InterpreterContext, ctx: HippoContext) {
  def during: String = "during pure evaluation"

  def eval(namespace: MutableNamespace, expr: HlangExpression): HlangValue = expr match {
    case e: HlangExpression.Const => e.value

    case e: HlangExpression.DlExpression => interpolateExpression(namespace, e).toHValue
    case e: HlangExpression.DlSequent => interpolateSequent(namespace, e).toHValue

    case e: HlangExpression.Import => throw HlangException(s"import not allowed $during", slice = e.slice)

    case e: HlangExpression.Declare =>
      for (slice <- e.exportSlice) throw HlangException(s"export not allowed $during", slice = slice)
      val value = eval(namespace, e.value)
      namespace.declare(e.name, value, e.mutable)
      value

    case e: HlangExpression.Assign =>
      val value = eval(namespace, e.value)
      HlangException.at(e.slice) { namespace.assign(e.name, value) }
      value

    case e: HlangExpression.AssignGoal => throw HlangException(s"goal assignment not allowed $during", slice = e.slice)

    case e: HlangExpression.Lookup => HlangException.at(e.slice) { namespace.lookup(e.name) }

    case e: HlangExpression.LookupGoal => throw HlangException(s"goal lookup not allowed $during", slice = e.slice)

    case e: HlangExpression.If =>
      val condition = eval(namespace, e.condition)
      if (condition.isTruthy) eval(namespace, e.ifTrue) else e.ifFalse.map(eval(namespace, _)).toHValue

    case e: HlangExpression.While =>
      var lastValue: HlangValue = HlangValue.Null
      while (true) {
        val condition = eval(namespace, e.condition)
        if (!condition.isTruthy) return lastValue
        lastValue = eval(namespace, e.body)
      }
      lastValue

    case e: HlangExpression.Function => HlangValue.Function(namespace.freeze, e.args, e.body)

    case e: HlangExpression.Theorem =>
      val conclusion = HlangException.at(e.conclusion.slice) { eval(namespace, e.conclusion).asSequent }
      val premises = e.premises.map(eval(namespace, _).asSequent).toIndexedSeq
      val proof = eval(namespace, e.proof)

      val proven = proof match {
        case HlangValue.Proof(value) => value
        case HlangValue.ProofInfo(value) => value.proof
        case HlangValue.Tactic(value) => ctx.tactic(Cached(value), conclusion, premises)
        case _ =>
          throw HlangException("must be a proof or a tactic", slice = e.proofSlice, label = "while proving theorem")
      }

      if (conclusion != proven.conclusion || premises != proven.premises.map(_.sequent)) {
        val declaredConclusion = SequentPrinter.oneline(conclusion.sequent)
        val declaredPremises = premises.map(p => s"\n  given ${SequentPrinter.oneline(p.sequent)}").mkString
        val provenConclusion = SequentPrinter.oneline(proven.conclusion.sequent)
        val provenPremises = proven
          .premises
          .map(p => s"\n  given ${SequentPrinter.oneline(p.sequent.sequent)}")
          .mkString
        throw HlangException(
          s"""Proof does not match theorem declaration:
             |
             |Declared:
             |  $declaredConclusion$declaredPremises
             |
             |Proven:
             |  $provenConclusion$provenPremises""".stripMargin,
          slice = e.slice,
          label = "while checking this theorem",
        )
      }

      for (slice <- e.verifySlice) HlangException.at(slice, "while verifying this theorem") {
        val provable = ctx.provableFromLocalProof(proven)
        require(provable.conclusion == proven.conclusion.sequentExpanded, "Provable conclusion does not match")
        require(provable.subgoals == proven.premises.map(_.sequent.sequentExpanded), "Provable subgoals don't match")
      }

      proven.toHValue

    case e: HlangExpression.Sequence =>
      for (expr <- e.exprs) eval(namespace, expr)
      e.returnExpr.map(eval(namespace, _)).toHValue

    case e: HlangExpression.Block =>
      val nestedNamespace = new MutableNamespace(Some(namespace))
      eval(nestedNamespace, e.inner)

    case e: HlangExpression.BackwardBlock =>
      HlangValue.Tactic(InterpreterBackward.tactic(ictx = ictx, namespace = namespace.freeze, expr = e))

    case e: HlangExpression.GraphBlock =>
      HlangValue.Tactic(InterpreterGraph.tactic(ictx = ictx, ctx = ctx, namespace = namespace.freeze, expr = e.inner))

    case e: HlangExpression.BuiltinAccess =>
      val target = eval(namespace, e.target)
      HlangValue.BuiltinMemberFunction(target, e.member)

    case e: HlangExpression.Access =>
      val target = eval(namespace, e.target)
      accessValue(e, target)

    case e: HlangExpression.Apply =>
      val target = eval(namespace, e.target)
      val args = e.args.map(eval(namespace, _))
      applyValue(e, target, args)

    case e: HlangExpression.ApplyTactic =>
      throw HlangException(s"tactic application not allowed $during", slice = e.slice)
  }

  // Protected because otherwise the value would have to be computed twice.
  protected def accessValue(e: HlangExpression.Access, target: HlangValue): HlangValue = {
    import org.keymaerax.hippolang.BuiltinMemberFunction.*

    (target, e.name.value) match {
      case (HlangValue.Namespace(ns), _) => HlangException.at(e.nameSlice) { ns.lookup(e.name) }
      case (HlangValue.Tactic(_), "forward") => HlangValue.BuiltinMemberFunction(target, Forward)
      case (HlangValue.Tactic(_), "backward") => HlangValue.BuiltinMemberFunction(target, Backward)
      case (HlangValue.Tactic(_), "pure") => HlangValue.BuiltinMemberFunction(target, Pure)
      case (HlangValue.DlExpression(_), "select") => HlangValue.BuiltinMemberFunction(target, Select)
      case (HlangValue.Proof(value), "join") => HlangValue.BuiltinMemberFunction(target, Join)
      case (HlangValue.Proof(value), "usubst") => HlangValue.BuiltinMemberFunction(target, Usubst)
      case (HlangValue.Proof(value), "urename") => HlangValue.BuiltinMemberFunction(target, Urename)
      case (HlangValue.Proof(value), "conclusion") => value.conclusion.toHValue
      case (HlangValue.Proof(value), "premises") => value.premises.map(_.sequent.toHValue).toHValue
      // TODO Better solution for proof/proofinfo duality
      case (HlangValue.ProofInfo(value), "join") => HlangValue.BuiltinMemberFunction(target, Join)
      case (HlangValue.ProofInfo(value), "usubst") => HlangValue.BuiltinMemberFunction(target, Usubst)
      case (HlangValue.ProofInfo(value), "urename") => HlangValue.BuiltinMemberFunction(target, Urename)
      case (HlangValue.ProofInfo(value), "conclusion") => value.proof.conclusion.toHValue
      case (HlangValue.ProofInfo(value), "premises") => value.proof.premises.map(_.sequent.toHValue).toHValue
      case (HlangValue.DlSequent(value), "ante") =>
        value.sequent.ante.map(HippoExpression(_, value.defs).toHValue).toHValue
      case (HlangValue.DlSequent(value), "succ") =>
        value.sequent.succ.map(HippoExpression(_, value.defs).toHValue).toHValue
      case (HlangValue.List(value), "length") => value.length.toHValue
      case _ => throw new UnsupportedOperationException("incorrect access")
    }
  }

  // Protected because otherwise the values would have to be computed twice.
  protected def applyValue(e: HlangExpression.Apply, target: HlangValue, args: IndexedSeq[HlangValue]): HlangValue =
    target match {
      case HlangValue.TacticInfo(value) => applyTacticInfo(e, value, args)
      case HlangValue.BuiltinFunction(value) => applyBuiltinFunction(e, value, args)
      case HlangValue.BuiltinMemberFunction(target, value) => applyBuiltinMemberFunction(e, target, value, args)
      case HlangValue.Function(env, argNames, body) => applyFunction(e, env, argNames, body, args)
      case HlangValue.List(value) => applyList(e, value, args)
      case _ => throw new IllegalArgumentException("can only apply builtin")
    }

  private def applyTacticInfo(e: HlangExpression.Apply, info: TacticInfo, args: IndexedSeq[HlangValue]): HlangValue = {
    val tacticArgs = args.map(InterpreterPure.hippoValToTacticArg)
    HlangException.at(e.argsSlice, "while constructing the tactic") {
      info.constructor.constructPositional(tacticArgs).toHValue
    }
  }

  private def applyBuiltinFunction(
      e: HlangExpression.Apply,
      target: BuiltinFunction,
      args: IndexedSeq[HlangValue],
  ): HlangValue = target match {
    case BuiltinFunction.Not =>
      val Seq(arg) = args
      (!arg.isTruthy).toHValue

    case BuiltinFunction.Neg =>
      val Seq(arg) = args
      (-arg.asInt).toHValue

    case BuiltinFunction.Mul =>
      val Seq(left, right) = args
      (left.asInt * right.asInt).toHValue

    case BuiltinFunction.Div =>
      val Seq(left, right) = args
      val leftI = left.asInt
      val rightI = right.asInt
      if (rightI == 0) throw HlangException("division by zero", e.args(1).slice, "this divisor is zero")
      (leftI / rightI).toHValue

    case BuiltinFunction.Add =>
      val Seq(left, right) = args
      (left.asInt + right.asInt).toHValue

    case BuiltinFunction.Sub =>
      val Seq(left, right) = args
      (left.asInt - right.asInt).toHValue

    case BuiltinFunction.Gt =>
      val Seq(left, right) = args
      (left.asInt > right.asInt).toHValue

    case BuiltinFunction.Gte =>
      val Seq(left, right) = args
      (left.asInt >= right.asInt).toHValue

    case BuiltinFunction.Lt =>
      val Seq(left, right) = args
      (left.asInt < right.asInt).toHValue

    case BuiltinFunction.Lte =>
      val Seq(left, right) = args
      (left.asInt <= right.asInt).toHValue

    case BuiltinFunction.Eq =>
      val Seq(left, right) = args
      (left == right).toHValue

    case BuiltinFunction.Neq =>
      val Seq(left, right) = args
      (left != right).toHValue

    case BuiltinFunction.And =>
      val Seq(left, right) = args
      if (left.isTruthy) right else left

    case BuiltinFunction.Or =>
      val Seq(left, right) = args
      if (left.isTruthy) left else right

    case BuiltinFunction.List => HlangValue.List(args)

    case BuiltinFunction.Proof =>
      val arg = getSingleArg(e, args, label = "while constructing proof")
      val sequent = getValueAsSequent(
        arg,
        "argument must be a sequent",
        slice = e.args(0).slice,
        label = "while constructing proof",
      )
      HlangValue.Proof(ctx.sequent(sequent))

    case BuiltinFunction.Print =>
      val parts = args.map {
        case HlangValue.String(v) => v
        case HlangValue.DlExpression(v) => PrettyPrinter.printHippoExpression(v)
        case HlangValue.DlSequent(v) => PrettyPrinter.printHippoSequent(v)
        case HlangValue.Proof(v) => PrettyPrinter.printHippoProof(v)
        case HlangValue.ProofInfo(v) => PrettyPrinter.printHippoProof(v.proof)
        case value => value.format
      }
      println(parts.mkString)
      HlangValue.Null

    case f @ BuiltinFunction.Premise =>
      throw new UnsupportedOperationException(s"#${f.name} can only be called in the context of a graph block")
  }

  private def applyBuiltinMemberFunction(
      e: HlangExpression.Apply,
      target: HlangValue,
      value: BuiltinMemberFunction,
      args: IndexedSeq[HlangValue],
  ): HlangValue = {
    import org.keymaerax.hippolang.BuiltinMemberFunction.*

    (target, value, args) match {
      case (HlangValue.Tactic(tactic: ForwardTactic), Forward, args) =>
        val premises = args.map(_.asSequent)
        ctx.forward(tactic, premises).toHValue

      // TODO Support premise hints
      case (HlangValue.Tactic(tactic: BackwardTactic), Backward, Seq(arg)) =>
        val conclusion = arg.asSequent
        ctx.backward(tactic, conclusion).toHValue

      case (HlangValue.Tactic(tactic: PureTactic), Pure, Seq()) => ctx.pure(tactic).toHValue

      case (HlangValue.ProofInfo(proof), Join, _) => applyBuiltinMemberFunction(e, proof.proof.toHValue, value, args)

      case (HlangValue.Proof(proof), Join, Seq(at, subproof)) =>
        val atV = getValueAsInt(
          at,
          message = "argument must be an integer",
          slice = e.args(0).slice,
          label = "while joining proofs",
        )

        val subproofV = getValueAsProof(
          subproof,
          message = "argument must be a proof",
          slice = e.args(1).slice,
          label = "while joining proofs",
        )

        HlangException.at(slice = e.slice, label = "while joining proofs") {
          ctx.joinAt(atV)(proof, subproofV).toHValue
        }

      case (HlangValue.ProofInfo(proof), Usubst, _) => applyBuiltinMemberFunction(e, proof.proof.toHValue, value, args)

      // TODO Support multiple substitution pairs
      case (HlangValue.Proof(proof), Usubst, Seq(from, to)) =>
        val fromV = getValueAsExpression(
          from,
          message = "argument must be a dL expression",
          slice = e.args(0).slice,
          label = "while performing usubst",
        )

        val toV = getValueAsExpression(
          to,
          message = "argument must be a dL expression",
          slice = e.args(0).slice,
          label = "while performing usubst",
        )

        HlangException.at(slice = e.slice, label = "while performing usubst") {
          ctx.uSubst(proof, fromV.exprExpanded -> toV.exprExpanded).toHValue
        }

      case (HlangValue.ProofInfo(proof), Urename, _) => applyBuiltinMemberFunction(e, proof.proof.toHValue, value, args)

      case (HlangValue.Proof(proof), Urename, Seq(from, to)) =>
        val fromV = getValueAsExpression(
          from,
          message = "argument must be a dL expression",
          slice = e.args(0).slice,
          label = "while performing urename",
        )

        val toV = getValueAsExpression(
          to,
          message = "argument must be a dL expression",
          slice = e.args(0).slice,
          label = "while performing urename",
        )

        HlangException.at(slice = e.slice, label = "while performing urename") {
          ctx.uRename(proof, fromV.asInstanceOf[Variable], toV.asInstanceOf[Variable]).toHValue
        }

      case (HlangValue.DlExpression(value), Select, args) =>
        val arg = getSingleArg(e, args, label = "while selecting subexpression")

        val path = getValueAsList(
          arg,
          message = "path must be a list",
          slice = e.args(0).slice,
          label = "while selecting subexpression",
        )

        val segments = path
          .zipWithIndex
          .map { case (segment, i) =>
            getValueAsInt(
              segment,
              message = s"path segment at index $i must be an int",
              slice = e.args(0).slice,
              label = "while selecting subexpression",
            )
          }

        ExprPath(segments.toList).select(value).toHValue

      case _ => throw new UnsupportedOperationException("incorrect builtin member function application")
    }
  }

  private def applyFunction(
      e: HlangExpression.Apply,
      env: ImmutableNamespace,
      argNames: Seq[HlangIdentifier],
      body: HlangExpression,
      args: IndexedSeq[HlangValue],
  ) = {
    require(argNames.length == args.length)
    val innerEnv = new MutableNamespace(Some(env))
    for ((name, arg) <- argNames.zip(args)) innerEnv.declare(name, arg, mutable = false)
    eval(innerEnv, body)
  }

  private def applyList(
      e: HlangExpression.Apply,
      list: IndexedSeq[HlangValue],
      args: IndexedSeq[HlangValue],
  ): HlangValue = {
    val arg = getSingleArg(e, args, label = "while indexing list")
    val index = getValueAsInt(
      arg,
      message = "list index must be an integer",
      slice = e.args(0).slice,
      label = "while indexing list",
    )

    if (list.isEmpty)
      throw HlangException("can't index empty list", slice = e.args(0).slice, label = "while indexing list")

    if (index < 0 || index >= list.length) throw HlangException(
      s"index ($index) must be within bounds (0 <= index < ${list.length})",
      slice = e.args(0).slice,
      label = "while indexing list",
    )

    list(index)
  }
}

object InterpreterPure {
  protected def hippoValToTacticArg(value: HlangValue): Any = value match {
    case HlangValue.Null => None
    case HlangValue.Bool(value) => value
    case HlangValue.Int(value) => value
    case HlangValue.String(value) => value
    case HlangValue.List(values) => values.map(hippoValToTacticArg)
    case HlangValue.DlExpression(value) => value
    case HlangValue.DlSequent(value) => value
    case HlangValue.Proof(value) => value
    case HlangValue.Tactic(value) => value
    case HlangValue.ProofInfo(value) => value
    case HlangValue.TacticInfo(value) => value
    case _ => throw new UnsupportedOperationException("can't convert value to tactic argument")
  }

  protected def getSingleArg(e: HlangExpression.Apply, args: Seq[HlangValue], label: String): HlangValue = args match {
    case Seq(arg) => arg
    case _ => throw HlangException("exactly one argument required", slice = e.argsSlice, label = label)
  }

  protected def getValueAsInt(value: HlangValue, message: String, slice: SourceFile#Slice, label: String): Int =
    value match {
      case HlangValue.Int(value) => value
      case _ => throw HlangException(message, slice = slice, label = label)
    }

  protected def getValueAsExpression(
      value: HlangValue,
      message: String,
      slice: SourceFile#Slice,
      label: String,
  ): HippoExpression = value match {
    case HlangValue.DlExpression(value) => value
    case HlangValue.Int(value) => HippoExpression(Number(value))
    case _ => throw HlangException(message, slice = slice, label = label)
  }

  protected def getValueAsTerm(
      value: HlangValue,
      message: String,
      slice: SourceFile#Slice,
      label: String,
  ): (Term, Definitions) = getValueAsExpression(value, message, slice, label) match {
    case HippoExpression(expr: Term, defs) => (expr, defs)
    case _ => throw HlangException(message, slice = slice, label = label)
  }

  protected def getValueAsFormula(
      value: HlangValue,
      message: String,
      slice: SourceFile#Slice,
      label: String,
  ): (Formula, Definitions) = getValueAsExpression(value, message, slice, label) match {
    case HippoExpression(expr: Formula, defs) => (expr, defs)
    case _ => throw HlangException(message, slice = slice, label = label)
  }

  protected def getValueAsProgram(
      value: HlangValue,
      message: String,
      slice: SourceFile#Slice,
      label: String,
  ): (Program, Definitions) = getValueAsExpression(value, message, slice, label) match {
    case HippoExpression(expr: Program, defs) => (expr, defs)
    case _ => throw HlangException(message, slice = slice, label = label)
  }

  protected def getValueAsSequent(
      value: HlangValue,
      message: String,
      slice: SourceFile#Slice,
      label: String,
  ): HippoSequent = value match {
    case HlangValue.DlSequent(value) => value
    case _ => throw HlangException(message, slice = slice, label = label)
  }

  protected def getValueAsProof(
      value: HlangValue,
      message: String,
      slice: SourceFile#Slice,
      label: String,
  ): HippoProof = value match {
    case HlangValue.Proof(value) => value
    case HlangValue.ProofInfo(value) => value.proof
    case _ => throw HlangException(message, slice = slice, label = label)
  }

  protected def getValueAsList(
      value: HlangValue,
      message: String,
      slice: SourceFile#Slice,
      label: String,
  ): IndexedSeq[HlangValue] = value match {
    case HlangValue.List(values) => values
    case _ => throw HlangException(message, slice = slice, label = label)
  }

  protected def interpolateExpression(namespace: MutableNamespace, e: HlangExpression.DlExpression): HippoExpression = {
    val slice = e.slice
    val label = "while evaluating dL expression"
    def errorMsg(name: String, msg: String): String = s"failed to interpolate $name: $msg"

    new Interpolator(
      lookup = { name =>
        val value = HlangException.at(slice, label) { namespace.lookup(HlangIdentifier(name)) }
        getValueAsExpression(
          value,
          errorMsg(name, "replacement value can't be converted to a dL expression"),
          slice,
          label,
        )
      },
      onError = (name, msg) => throw HlangException(errorMsg(name, msg), slice, label),
    ).interpolate(e.value)
  }

  protected def interpolateSequent(namespace: MutableNamespace, e: HlangExpression.DlSequent): HippoSequent = {
    val slice = e.slice
    val label = "while evaluating dL sequent"

    def errorMsg(name: String, msg: String): String = s"failed to interpolate $name: $msg"

    new Interpolator(
      lookup = { name =>
        val value = HlangException.at(slice, label) { namespace.lookup(HlangIdentifier(name)) }
        getValueAsExpression(
          value,
          errorMsg(name, "replacement value can't be converted to a dL sequent"),
          slice,
          label,
        )
      },
      onError = (name, msg) => throw HlangException(errorMsg(name, msg), slice, label),
    ).interpolate(e.value)
  }
}
