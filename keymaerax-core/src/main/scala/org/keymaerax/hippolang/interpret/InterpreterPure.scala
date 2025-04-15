/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.core.{Expression, Formula, Number, Program, Sequent, Term, Variable}
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{ExprPath, SequentPrinter}
import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic, PureTactic}
import org.keymaerax.hippolang.HippoConversions.*
import org.keymaerax.hippolang.interpret.InterpreterPure.{
  getSingleArg,
  getValueAsExpression,
  getValueAsInt,
  getValueAsList,
  getValueAsProof,
  getValueAsSequent,
  interpolateExpression,
}
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.parse.SourceFile
import org.keymaerax.hippolang.{
  BuiltinFunction,
  BuiltinMemberFunction,
  HippoExpression,
  HippoIdentifier,
  HippoValue,
  HlangException,
}
import org.keymaerax.hippolib.meta.TacticInfo
import org.keymaerax.hippolib.primitive.Cached

class InterpreterPure(ictx: HippoInterpreterContext, ctx: HippoContext) {
  def during: String = "during pure evaluation"

  def eval(namespace: MutableNamespace, expr: HippoExpression): HippoValue = expr match {
    case e: HippoExpression.Const => e.value

    case e: HippoExpression.DlExpression => interpolateExpression(namespace, e).toHValue

    case e: HippoExpression.Import => throw HlangException(s"import not allowed $during", slice = e.slice)

    case e: HippoExpression.Declare =>
      for (slice <- e.exportSlice) throw HlangException(s"export not allowed $during", slice = slice)
      val value = eval(namespace, e.value)
      namespace.declare(e.name, value, e.mutable)
      value

    case e: HippoExpression.Assign =>
      val value = eval(namespace, e.value)
      HlangException.at(e.slice) { namespace.assign(e.name, value) }
      value

    case e: HippoExpression.AssignGoal => throw HlangException(s"goal assignment not allowed $during", slice = e.slice)

    case e: HippoExpression.Lookup => HlangException.at(e.slice) { namespace.lookup(e.name) }

    case e: HippoExpression.LookupGoal => throw HlangException(s"goal lookup not allowed $during", slice = e.slice)

    case e: HippoExpression.If =>
      val condition = eval(namespace, e.condition)
      if (condition.isTruthy) eval(namespace, e.ifTrue) else e.ifFalse.map(eval(namespace, _)).toHValue

    case e: HippoExpression.While =>
      var lastValue: HippoValue = HippoValue.Null
      while (true) {
        val condition = eval(namespace, e.condition)
        if (!condition.isTruthy) return lastValue
        lastValue = eval(namespace, e.body)
      }
      lastValue

    case e: HippoExpression.Function => HippoValue.Function(namespace.freeze, e.args, e.body)

    case e: HippoExpression.Theorem =>
      val conclusion = eval(namespace, e.conclusion).asSequent
      val premises = e.premises.map(eval(namespace, _).asSequent).toIndexedSeq
      val proof = eval(namespace, e.proof)

      val proven = proof match {
        case HippoValue.Proof(value) => value
        case HippoValue.ProofInfo(value) => value.proof
        case HippoValue.Tactic(value) => HlangException
            .at(e.proofSlice, "while running this tactic") { ctx.tactic(Cached(value), conclusion, premises) }
        case _ =>
          throw HlangException("must be a proof or a tactic", slice = e.proofSlice, label = "while proving theorem")
      }

      if (conclusion != proven.conclusion || premises != proven.premises.map(_.sequent)) {
        val declaredConclusion = SequentPrinter.oneline(conclusion)
        val declaredPremises = premises.map(p => s"\n  given ${SequentPrinter.oneline(p)}").mkString
        val provenConclusion = SequentPrinter.oneline(proven.conclusion)
        val provenPremises = proven.premises.map(p => s"\n  given ${SequentPrinter.oneline(p.sequent)}").mkString
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
        require(provable.conclusion == proven.conclusion, "Provable conclusion does not match")
        require(provable.subgoals == proven.premises.map(_.sequent), "Provable subgoals don't match")
      }

      proven.toHValue

    case e: HippoExpression.Sequence =>
      for (expr <- e.exprs) eval(namespace, expr)
      e.returnExpr.map(eval(namespace, _)).toHValue

    case e: HippoExpression.Block =>
      val nestedNamespace = new MutableNamespace(Some(namespace))
      eval(nestedNamespace, e.inner)

    case e: HippoExpression.BackwardBlock =>
      HippoValue.Tactic(InterpreterBackward.tactic(ictx = ictx, namespace = namespace.freeze, expr = e))

    case e: HippoExpression.GraphBlock =>
      HippoValue.Tactic(InterpreterGraph.tactic(ictx = ictx, ctx = ctx, namespace = namespace.freeze, expr = e.inner))

    case e: HippoExpression.BuiltinAccess =>
      val target = eval(namespace, e.target)
      HippoValue.BuiltinMemberFunction(target, e.member)

    case e: HippoExpression.Access =>
      val target = eval(namespace, e.target)
      accessValue(e, target)

    case e: HippoExpression.Apply =>
      val target = eval(namespace, e.target)
      val args = e.args.map(eval(namespace, _))
      applyValue(e, target, args)

    case e: HippoExpression.ApplyTactic =>
      throw HlangException(s"tactic application not allowed $during", slice = e.slice)
  }

  // Protected because otherwise the value would have to be computed twice.
  protected def accessValue(e: HippoExpression.Access, target: HippoValue): HippoValue = {
    import org.keymaerax.hippolang.BuiltinMemberFunction.*

    (target, e.name.value) match {
      case (HippoValue.Namespace(ns), _) => HlangException.at(e.nameSlice) { ns.lookup(e.name) }
      case (HippoValue.Tactic(_), "forward") => HippoValue.BuiltinMemberFunction(target, Forward)
      case (HippoValue.Tactic(_), "backward") => HippoValue.BuiltinMemberFunction(target, Backward)
      case (HippoValue.Tactic(_), "pure") => HippoValue.BuiltinMemberFunction(target, Pure)
      case (HippoValue.DlExpression(_), "select") => HippoValue.BuiltinMemberFunction(target, Select)
      case (HippoValue.Proof(value), "join") => HippoValue.BuiltinMemberFunction(target, Join)
      case (HippoValue.Proof(value), "usubst") => HippoValue.BuiltinMemberFunction(target, Usubst)
      case (HippoValue.Proof(value), "urename") => HippoValue.BuiltinMemberFunction(target, Urename)
      case (HippoValue.Proof(value), "conclusion") => value.conclusion.toHValue
      case (HippoValue.Proof(value), "premises") => value.premises.map(_.sequent.toHValue).toHValue
      // TODO Better solution for proof/proofinfo duality
      case (HippoValue.ProofInfo(value), "join") => HippoValue.BuiltinMemberFunction(target, Join)
      case (HippoValue.ProofInfo(value), "usubst") => HippoValue.BuiltinMemberFunction(target, Usubst)
      case (HippoValue.ProofInfo(value), "urename") => HippoValue.BuiltinMemberFunction(target, Urename)
      case (HippoValue.ProofInfo(value), "conclusion") => value.proof.conclusion.toHValue
      case (HippoValue.ProofInfo(value), "premises") => value.proof.premises.map(_.sequent.toHValue).toHValue
      case (HippoValue.DlSequent(value), "ante") => value.ante.map(_.toHValue).toHValue
      case (HippoValue.DlSequent(value), "succ") => value.succ.map(_.toHValue).toHValue
      case (HippoValue.List(value), "length") => value.length.toHValue
      case _ => throw new UnsupportedOperationException("incorrect access")
    }
  }

  // Protected because otherwise the values would have to be computed twice.
  protected def applyValue(e: HippoExpression.Apply, target: HippoValue, args: IndexedSeq[HippoValue]): HippoValue =
    target match {
      case HippoValue.TacticInfo(value) => applyTacticInfo(e, value, args)
      case HippoValue.BuiltinFunction(value) => applyBuiltinFunction(e, value, args)
      case HippoValue.BuiltinMemberFunction(target, value) => applyBuiltinMemberFunction(e, target, value, args)
      case HippoValue.Function(env, argNames, body) => applyFunction(e, env, argNames, body, args)
      case HippoValue.List(value) => applyList(e, value, args)
      case _ => throw new IllegalArgumentException("can only apply builtin")
    }

  private def applyTacticInfo(e: HippoExpression.Apply, info: TacticInfo, args: IndexedSeq[HippoValue]): HippoValue = {
    val tacticArgs = args.map(InterpreterPure.hippoValToTacticArg)
    HlangException.at(e.argsSlice, "while constructing the tactic") {
      info.constructor.constructPositional(tacticArgs).toHValue
    }
  }

  private def applyBuiltinFunction(
      e: HippoExpression.Apply,
      target: BuiltinFunction,
      args: IndexedSeq[HippoValue],
  ): HippoValue = target match {
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

    case BuiltinFunction.List => HippoValue.List(args)

    case BuiltinFunction.Proof =>
      val arg = getSingleArg(e, args, label = "while constructing proof")
      val sequent = getValueAsSequent(
        arg,
        "argument must be a sequent",
        slice = e.args(0).slice,
        label = "while constructing proof",
      )
      HippoValue.Proof(ctx.sequent(sequent))

    case BuiltinFunction.Print =>
      val parts = args.map {
        case HippoValue.String(str) => str
        case value => value.format
      }
      println(parts.mkString)
      HippoValue.Null

    case f @ BuiltinFunction.Premise =>
      throw new UnsupportedOperationException(s"#${f.name} can only be called in the context of a graph block")
  }

  private def applyBuiltinMemberFunction(
      e: HippoExpression.Apply,
      target: HippoValue,
      value: BuiltinMemberFunction,
      args: IndexedSeq[HippoValue],
  ): HippoValue = {
    import org.keymaerax.hippolang.BuiltinMemberFunction.*

    (target, value, args) match {
      case (HippoValue.Tactic(tactic: ForwardTactic), Forward, args) =>
        val premises = args.map(_.asSequent)
        ctx.forward(tactic, premises).toHValue

      // TODO Support premise hints
      case (HippoValue.Tactic(tactic: BackwardTactic), Backward, Seq(arg)) =>
        val conclusion = arg.asSequent
        ctx.backward(tactic, conclusion).toHValue

      case (HippoValue.Tactic(tactic: PureTactic), Pure, Seq()) => ctx.pure(tactic).toHValue

      case (HippoValue.ProofInfo(proof), Join, _) => applyBuiltinMemberFunction(e, proof.proof.toHValue, value, args)

      case (HippoValue.Proof(proof), Join, Seq(at, subproof)) =>
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

      case (HippoValue.ProofInfo(proof), Usubst, _) => applyBuiltinMemberFunction(e, proof.proof.toHValue, value, args)

      // TODO Support multiple substitution pairs
      case (HippoValue.Proof(proof), Usubst, Seq(from, to)) =>
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
          ctx.uSubst(proof, fromV -> toV).toHValue
        }

      case (HippoValue.ProofInfo(proof), Urename, _) => applyBuiltinMemberFunction(e, proof.proof.toHValue, value, args)

      case (HippoValue.Proof(proof), Urename, Seq(from, to)) =>
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

      case (HippoValue.DlExpression(value), Select, args) =>
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
      e: HippoExpression.Apply,
      env: ImmutableNamespace,
      argNames: Seq[HippoIdentifier],
      body: HippoExpression,
      args: IndexedSeq[HippoValue],
  ) = {
    require(argNames.length == args.length)
    val innerEnv = new MutableNamespace(Some(env))
    for ((name, arg) <- argNames.zip(args)) innerEnv.declare(name, arg, mutable = false)
    eval(innerEnv, body)
  }

  private def applyList(
      e: HippoExpression.Apply,
      list: IndexedSeq[HippoValue],
      args: IndexedSeq[HippoValue],
  ): HippoValue = {
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
  protected def hippoValToTacticArg(value: HippoValue): Any = value match {
    case HippoValue.Null => None
    case HippoValue.Bool(value) => value
    case HippoValue.Int(value) => value
    case HippoValue.String(value) => value
    case HippoValue.List(values) => values.map(hippoValToTacticArg)
    case HippoValue.DlExpression(value) => value
    case HippoValue.DlSequent(value) => value
    case HippoValue.Proof(value) => value
    case HippoValue.Tactic(value) => value
    case HippoValue.ProofInfo(value) => value
    case HippoValue.TacticInfo(value) => value
    case _ => throw new UnsupportedOperationException("can't convert value to tactic argument")
  }

  protected def getSingleArg(e: HippoExpression.Apply, args: Seq[HippoValue], label: String): HippoValue = args match {
    case Seq(arg) => arg
    case _ => throw HlangException("exactly one argument required", slice = e.argsSlice, label = label)
  }

  protected def getValueAsInt(value: HippoValue, message: String, slice: SourceFile#Slice, label: String): Int =
    value match {
      case HippoValue.Int(value) => value
      case _ => throw HlangException(message, slice = slice, label = label)
    }

  protected def getValueAsExpression(
      value: HippoValue,
      message: String,
      slice: SourceFile#Slice,
      label: String,
  ): Expression = value match {
    case HippoValue.DlExpression(value) => value
    case HippoValue.Int(value) => Number(value)
    case _ => throw HlangException(message, slice = slice, label = label)
  }

  protected def getValueAsTerm(value: HippoValue, message: String, slice: SourceFile#Slice, label: String): Term =
    getValueAsExpression(value, message, slice, label) match {
      case value: Term => value
      case _ => throw HlangException(message, slice = slice, label = label)
    }

  protected def getValueAsFormula(value: HippoValue, message: String, slice: SourceFile#Slice, label: String): Formula =
    getValueAsExpression(value, message, slice, label) match {
      case value: Formula => value
      case _ => throw HlangException(message, slice = slice, label = label)
    }

  protected def getValueAsProgram(value: HippoValue, message: String, slice: SourceFile#Slice, label: String): Program =
    getValueAsExpression(value, message, slice, label) match {
      case value: Program => value
      case _ => throw HlangException(message, slice = slice, label = label)
    }

  protected def getValueAsSequent(value: HippoValue, message: String, slice: SourceFile#Slice, label: String): Sequent =
    value match {
      case HippoValue.DlSequent(value) => value
      case _ => throw HlangException(message, slice = slice, label = label)
    }

  protected def getValueAsProof(
      value: HippoValue,
      message: String,
      slice: SourceFile#Slice,
      label: String,
  ): HippoProof = value match {
    case HippoValue.Proof(value) => value
    case HippoValue.ProofInfo(value) => value.proof
    case _ => throw HlangException(message, slice = slice, label = label)
  }

  protected def getValueAsList(
      value: HippoValue,
      message: String,
      slice: SourceFile#Slice,
      label: String,
  ): IndexedSeq[HippoValue] = value match {
    case HippoValue.List(values) => values
    case _ => throw HlangException(message, slice = slice, label = label)
  }

  protected def interpolateExpression(namespace: MutableNamespace, e: HippoExpression.DlExpression): Expression = {
    val slice = e.slice
    val label = "while evaluating dL expression"
    def errorMsg(name: String, msg: String): String = s"failed to interpolate $name: $msg"

    new Interpolator(
      lookup = { name =>
        val value = HlangException.at(slice, label) { namespace.lookup(HippoIdentifier(name)) }
        getValueAsExpression(
          value,
          errorMsg(name, "replacement value can't be converted to a dL expression"),
          slice,
          label,
        )
      },
      onError = (name, msg) => throw HlangException(errorMsg(name, msg), slice, label),
    ).interpolate(expression = e.value)
  }
}
