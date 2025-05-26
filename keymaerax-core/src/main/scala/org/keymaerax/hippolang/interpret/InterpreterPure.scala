/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.core.Variable
import org.keymaerax.hippocore.proof.{HippoExpression, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{ExprPath, SequentPrinter}
import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic, PureTactic}
import org.keymaerax.hippolang.HlangConversions.*
import org.keymaerax.hippolang.interpret.InterpreterPure.{
  getArgValueAsTacticArg,
  getArgValuesAsTacticArgs,
  getOneArg,
  getTwoArgs,
  getZeroArgs,
  interpolateExpression,
  interpolateSequent,
}
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.{
  BuiltinFunction,
  BuiltinMemberFunction,
  HlangException,
  HlangExpression,
  HlangIdentifier,
  HlangValue,
  PrettyPrinter,
}
import org.keymaerax.hippolib.meta.{TacticArg, TacticInfo}
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

    case e: HlangExpression.Spread =>
      throw HlangException(s"spreading not allowed in this context $during", slice = e.slice)

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
  protected def accessValue(e: HlangExpression.Access, target: HlangValue): HlangValue = HlangException
    .at(e.slice, "while accessing value") {
      import org.keymaerax.hippolang.BuiltinMemberFunction.*

      (e.name, target) match {
        case (name, HlangValue.Namespace(v)) => HlangException.at(e.nameSlice) { v.lookup(name) }
        case (_, HlangValue.ProofInfo(v)) => accessValue(e, HlangValue.Proof(v.proof))

        // Member values
        case (HlangIdentifier("conclusion"), HlangValue.Proof(v)) => v.conclusion.toHValue
        case (HlangIdentifier("premises"), HlangValue.Proof(v)) => v.premises.map(_.sequent.toHValue).toHValue
        case (HlangIdentifier("ante"), HlangValue.DlSequent(v)) => v.antes.map(_.toHValue).toHValue
        case (HlangIdentifier("succ"), HlangValue.DlSequent(v)) => v.succs.map(_.toHValue).toHValue
        case (HlangIdentifier("length"), HlangValue.List(v)) => v.length.toHValue

        // Builtin member functions
        case (Forward.name, _: HlangValue.Tactic) => HlangValue.BuiltinMemberFunction(target, Forward)
        case (Backward.name, _: HlangValue.Tactic) => HlangValue.BuiltinMemberFunction(target, Backward)
        case (Pure.name, _: HlangValue.Tactic) => HlangValue.BuiltinMemberFunction(target, Pure)
        case (Join.name, _: HlangValue.Proof) => HlangValue.BuiltinMemberFunction(target, Join)
        case (Usubst.name, _: HlangValue.Proof) => HlangValue.BuiltinMemberFunction(target, Usubst)
        case (Urename.name, _: HlangValue.Proof) => HlangValue.BuiltinMemberFunction(target, Urename)
        case (Select.name, _: HlangValue.DlExpression) => HlangValue.BuiltinMemberFunction(target, Select)
        case (Expand.name, _: HlangValue.DlExpression) => HlangValue.BuiltinMemberFunction(target, Expand)
        case (Expand.name, _: HlangValue.DlSequent) => HlangValue.BuiltinMemberFunction(target, Expand)
        case (ExpandAll.name, _: HlangValue.DlExpression) => HlangValue.BuiltinMemberFunction(target, ExpandAll)
        case (ExpandAll.name, _: HlangValue.DlSequent) => HlangValue.BuiltinMemberFunction(target, ExpandAll)

        case _ => throw HlangException("invalid member access")
      }
    }

  // Protected because otherwise the values would have to be computed twice.
  protected def applyValue(e: HlangExpression.Apply, target: HlangValue, args: IndexedSeq[HlangValue]): HlangValue =
    target match {
      case HlangValue.TacticInfo(value) =>
        HlangException.at(e.slice, "while constructing tactic") { applyTacticInfo(e, value, args) }

      case HlangValue.BuiltinFunction(value) =>
        HlangException.at(e.slice, "while calling builtin function") { applyBuiltinFunction(e, value, args) }

      case HlangValue.BuiltinMemberFunction(target, value) => HlangException
          .at(e.slice, "while calling builtin member function") { applyBuiltinMemberFunction(e, target, value, args) }

      case HlangValue.Function(env, argNames, body) =>
        HlangException.at(e.slice, "while calling function") { applyFunction(e, env, argNames, body, args) }

      case HlangValue.List(value) => HlangException.at(e.slice, "while accessing list") { applyList(e, value, args) }

      case v =>
        throw HlangException(s"${v.getClass.getSimpleName} can't be called", e.slice, "while applying arguments")
    }

  private def applyTacticInfo(e: HlangExpression.Apply, info: TacticInfo, args: IndexedSeq[HlangValue]): HlangValue = {
    val tacticArgs = args.map(InterpreterPure.hippoValToTacticArg)
    info.constructor.constructPositional(tacticArgs).toHValue
  }

  private def applyBuiltinFunction(
      e: HlangExpression.Apply,
      target: BuiltinFunction,
      args: IndexedSeq[HlangValue],
  ): HlangValue = target match {
    case BuiltinFunction.Not =>
      val arg = getOneArg(args)
      (!arg.isTruthy).toHValue

    case BuiltinFunction.Neg =>
      val arg = getOneArg(args)
      (-arg.asInt).toHValue

    case BuiltinFunction.Mul =>
      val (left, right) = getTwoArgs(args)
      val leftV = getArgValueAsTacticArg(e.args(0), left, TacticArg.Int)
      val rightV = getArgValueAsTacticArg(e.args(1), right, TacticArg.Int)
      (leftV * rightV).toHValue

    case BuiltinFunction.Div =>
      val (left, right) = getTwoArgs(args)
      val leftV = getArgValueAsTacticArg(e.args(0), left, TacticArg.Int)
      val rightV = getArgValueAsTacticArg(e.args(1), right, TacticArg.Int)
      if (rightV == 0) throw HlangException("division by zero", e.args(1).slice, "this divisor is zero")
      (leftV / rightV).toHValue

    case BuiltinFunction.Add =>
      val (left, right) = getTwoArgs(args)
      val leftV = getArgValueAsTacticArg(e.args(0), left, TacticArg.Int)
      val rightV = getArgValueAsTacticArg(e.args(1), right, TacticArg.Int)
      (leftV + rightV).toHValue

    case BuiltinFunction.Sub =>
      val (left, right) = getTwoArgs(args)
      val leftV = getArgValueAsTacticArg(e.args(0), left, TacticArg.Int)
      val rightV = getArgValueAsTacticArg(e.args(1), right, TacticArg.Int)
      (leftV - rightV).toHValue

    case BuiltinFunction.Gt =>
      val (left, right) = getTwoArgs(args)
      val leftV = getArgValueAsTacticArg(e.args(0), left, TacticArg.Int)
      val rightV = getArgValueAsTacticArg(e.args(1), right, TacticArg.Int)
      (leftV > rightV).toHValue

    case BuiltinFunction.Gte =>
      val (left, right) = getTwoArgs(args)
      val leftV = getArgValueAsTacticArg(e.args(0), left, TacticArg.Int)
      val rightV = getArgValueAsTacticArg(e.args(1), right, TacticArg.Int)
      (leftV >= rightV).toHValue

    case BuiltinFunction.Lt =>
      val (left, right) = getTwoArgs(args)
      val leftV = getArgValueAsTacticArg(e.args(0), left, TacticArg.Int)
      val rightV = getArgValueAsTacticArg(e.args(1), right, TacticArg.Int)
      (leftV < rightV).toHValue

    case BuiltinFunction.Lte =>
      val (left, right) = getTwoArgs(args)
      val leftV = getArgValueAsTacticArg(e.args(0), left, TacticArg.Int)
      val rightV = getArgValueAsTacticArg(e.args(1), right, TacticArg.Int)
      (leftV <= rightV).toHValue

    case BuiltinFunction.Eq =>
      val (left, right) = getTwoArgs(args)
      (left == right).toHValue

    case BuiltinFunction.Neq =>
      val (left, right) = getTwoArgs(args)
      val leftV = getArgValueAsTacticArg(e.args(0), left, TacticArg.Int)
      val rightV = getArgValueAsTacticArg(e.args(1), right, TacticArg.Int)
      (leftV != rightV).toHValue

    case BuiltinFunction.And =>
      val (left, right) = getTwoArgs(args)
      if (left.isTruthy) right else left

    case BuiltinFunction.Or =>
      val (left, right) = getTwoArgs(args)
      if (left.isTruthy) left else right

    case BuiltinFunction.List => HlangValue.List(args)

    case BuiltinFunction.Proof =>
      val sequent = getOneArg(args)
      val sequentV = getArgValueAsTacticArg(e.args(0), sequent, TacticArg.HippoSequent)
      HlangValue.Proof(ctx.sequent(sequentV))

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
      throw HlangException(s"#${f.name} can only be called in the context of a graph block")
  }

  private def applyBuiltinMemberFunction(
      e: HlangExpression.Apply,
      target: HlangValue,
      value: BuiltinMemberFunction,
      args: IndexedSeq[HlangValue],
  ): HlangValue = {
    import org.keymaerax.hippolang.BuiltinMemberFunction.*

    (value, target) match {
      case (Forward, HlangValue.Tactic(tactic: ForwardTactic)) =>
        val premises = getArgValuesAsTacticArgs(e.args, args, TacticArg.HippoSequent)
        ctx.forward(tactic, premises.toIndexedSeq).toHValue

      // TODO Support premise hints
      case (Backward, HlangValue.Tactic(tactic: BackwardTactic)) =>
        val conclusion = getOneArg(args)
        val conclusionV = getArgValueAsTacticArg(e.args(0), conclusion, TacticArg.HippoSequent)
        ctx.backward(tactic, conclusionV).toHValue

      case (Pure, HlangValue.Tactic(tactic: PureTactic)) =>
        getZeroArgs(args)
        ctx.pure(tactic).toHValue

      case (Join, HlangValue.ProofInfo(proof)) => applyBuiltinMemberFunction(e, proof.proof.toHValue, value, args)

      case (Join, HlangValue.Proof(proof)) =>
        val (at, subproof) = getTwoArgs(args)
        val atV = getArgValueAsTacticArg(e.args(0), at, TacticArg.Int)
        val subproofV = getArgValueAsTacticArg(e.args(0), subproof, TacticArg.HippoProof)
        ctx.joinAt(atV)(proof, subproofV).toHValue

      case (Usubst, HlangValue.ProofInfo(proof)) => applyBuiltinMemberFunction(e, proof.proof.toHValue, value, args)

      // TODO Support multiple substitution pairs
      case (Usubst, HlangValue.Proof(proof)) =>
        val (from, to) = getTwoArgs(args)
        val fromV = getArgValueAsTacticArg(e.args(0), from, TacticArg.HippoExpression)
        val toV = getArgValueAsTacticArg(e.args(0), to, TacticArg.HippoExpression)
        ctx.uSubst(proof, fromV.exprExpanded -> toV.exprExpanded).toHValue

      case (Urename, HlangValue.ProofInfo(proof)) => applyBuiltinMemberFunction(e, proof.proof.toHValue, value, args)

      case (Urename, HlangValue.Proof(proof)) =>
        val (from, to) = getTwoArgs(args)
        val fromV = getArgValueAsTacticArg(e.args(0), from, TacticArg.HippoExpression)
        val toV = getArgValueAsTacticArg(e.args(0), to, TacticArg.HippoExpression)
        ctx.uRename(proof, fromV.asInstanceOf[Variable], toV.asInstanceOf[Variable]).toHValue

      case (Select, HlangValue.DlExpression(value)) =>
        val path = getOneArg(args)
        val pathV = getArgValueAsTacticArg(e.args(0), path, TacticArg.Seq(TacticArg.Int))
        ExprPath(pathV.toList).select(value).toHValue

      case (Expand, HlangValue.DlExpression(value)) =>
        val name = getOneArg(args)
        val nameV = getArgValueAsTacticArg(e.args(0), name, TacticArg.Name)
        value.expand(nameV).toHValue

      case (Expand, HlangValue.DlSequent(value)) =>
        val name = getOneArg(args)
        val nameV = getArgValueAsTacticArg(e.args(0), name, TacticArg.Name)
        value.expand(nameV).toHValue

      case (ExpandAll, HlangValue.DlExpression(value)) =>
        getZeroArgs(args)
        value.expandAll.toHValue

      case (ExpandAll, HlangValue.DlSequent(value)) =>
        getZeroArgs(args)
        value.expandAll.toHValue

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
    val arg = getOneArg(args)
    val index = getArgValueAsTacticArg(e.args(0), arg, TacticArg.Int)
    if (list.isEmpty) throw HlangException("can't index empty list")
    if (index < 0 || index >= list.length)
      throw HlangException(s"index $index must be within bounds (0 <= index < ${list.length})")
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

  protected def getValueAsTacticArg(value: HlangValue, arg: TacticArg): arg.Type = arg
    .validate(hippoValToTacticArg(value))

  protected def getArgValueAsTacticArg(e: HlangExpression, value: HlangValue, arg: TacticArg): arg.Type = HlangException
    .at(e.slice) { getValueAsTacticArg(value, arg) }

  protected def getArgValuesAsTacticArgs(
      exprs: Seq[HlangExpression],
      values: Seq[HlangValue],
      arg: TacticArg,
  ): Seq[arg.Type] = values
    .zip(exprs)
    .map { case (value, e) => HlangException.at(e.slice) { getValueAsTacticArg(value, arg) } }

  protected def getZeroArgs(args: Seq[HlangValue]): Unit = args match {
    case Seq() => ()
    case _ => throw HlangException("exactly zero arguments required")
  }

  protected def getOneArg(args: Seq[HlangValue]): HlangValue = args match {
    case Seq(arg) => arg
    case _ => throw HlangException("exactly one argument required")
  }

  protected def getTwoArgs(args: Seq[HlangValue]): (HlangValue, HlangValue) = args match {
    case Seq(arg1, arg2) => (arg1, arg2)
    case _ => throw HlangException("exactly two arguments required")
  }

  protected def interpolateExpression(namespace: MutableNamespace, e: HlangExpression.DlExpression): HippoExpression = {
    if (e.raw) return HippoExpression(e.value)

    val slice = e.slice
    val label = "while evaluating dL expression"
    def errorMsg(name: String, msg: String): String = s"failed to interpolate $name: $msg"

    new Interpolator(
      lookup = { name =>
        HlangException.at(slice, label) {
          val value = namespace.lookup(HlangIdentifier(name))
          getValueAsTacticArg(value, TacticArg.HippoExpression)
        }
      },
      onError = (name, msg) => throw HlangException(errorMsg(name, msg), slice, label),
    ).interpolate(e.value)
  }

  protected def interpolateSequent(namespace: MutableNamespace, e: HlangExpression.DlSequent): HippoSequent = {
    if (e.raw) return HippoSequent(e.value)

    val slice = e.slice
    val label = "while evaluating dL sequent"

    def errorMsg(name: String, msg: String): String = s"failed to interpolate $name: $msg"

    new Interpolator(
      lookup = { name =>
        HlangException.at(slice, label) {
          val value = namespace.lookup(HlangIdentifier(name))
          getValueAsTacticArg(value, TacticArg.HippoExpression)
        }
      },
      onError = (name, msg) => throw HlangException(errorMsg(name, msg), slice, label),
    ).interpolate(e.value)
  }
}
