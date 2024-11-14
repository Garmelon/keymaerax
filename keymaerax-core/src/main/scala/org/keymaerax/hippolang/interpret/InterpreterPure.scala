/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippolang.HippoConversions._
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.{BuiltinFunction, BuiltinMemberFunction, HippoExpression, HippoIdentifier, HippoValue}
import org.keymaerax.hippolib.meta.TacticInfo
import org.keymaerax.hippolib.primitive.Cached
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.{BackwardTactic, ForwardTactic, PureTactic}

class InterpreterPure(ictx: HippoInterpreterContext, ctx: HippoContext) {
  def eval(namespace: MutableNamespace, expr: HippoExpression): HippoValue = expr match {
    case HippoExpression.Const(value) => value

    case HippoExpression.Import(_) =>
      throw new UnsupportedOperationException("import not allowed during pure evaluation")

    case HippoExpression.Declare(true, _, _, _) =>
      throw new UnsupportedOperationException("export not allowed during pure evaluation")

    case HippoExpression.Declare(false, mutable, name, value) =>
      val valueV = eval(namespace, value)
      namespace.declare(name, valueV, mutable)
      valueV

    case HippoExpression.Assign(name, value) =>
      val valueV = eval(namespace, value)
      namespace.assign(name, valueV)
      valueV

    case HippoExpression.Lookup(name) => namespace.lookup(name)

    case HippoExpression.If(condition, ifTrue, ifFalse) =>
      val conditionV = eval(namespace, condition)
      if (conditionV.isTruthy) eval(namespace, ifTrue) else ifFalse.map(eval(namespace, _)).toHValue

    case HippoExpression.While(condition, body) =>
      var lastValue: HippoValue = HippoValue.Null
      while (true) {
        val conditionV = eval(namespace, condition)
        if (!conditionV.isTruthy) return lastValue
        lastValue = eval(namespace, body)
      }
      lastValue

    case HippoExpression.Function(args, body) => HippoValue.Function(namespace.freeze, args, body)

    case HippoExpression.Theorem(verified, conclusion, premises, proof) =>
      val conclusionV = eval(namespace, conclusion).asSequent
      val premisesV = premises.map(eval(namespace, _).asSequent).toIndexedSeq
      val proofV = eval(namespace, proof).asTactic
      val proven = ctx.tactic(Cached(proofV), conclusionV, premisesV)
      if (verified) {
        val provable = ctx.provableFromLocalProof(proven)
        require(provable.conclusion == proven.conclusion)
        require(provable.subgoals == proven.premises.map(_.sequent))
      }
      proven.toHValue

    case HippoExpression.Sequence(exprs, returnExpr) =>
      for (expr <- exprs) eval(namespace, expr)
      returnExpr.map(eval(namespace, _)).toHValue

    case HippoExpression.Block(inner) =>
      val nestedNamespace = new MutableNamespace(Some(namespace))
      eval(nestedNamespace, inner)

    case HippoExpression.BackwardBlock(inner) =>
      HippoValue.Tactic(InterpreterBackward.tactic(ictx = ictx, namespace = namespace.freeze, expr = inner))

    case HippoExpression.GraphBlock(inner) =>
      HippoValue.Tactic(InterpreterGraph.tactic(ictx = ictx, ctx = ctx, namespace = namespace.freeze, expr = inner))

    case HippoExpression.BuiltinAccess(target, member) =>
      val targetV = eval(namespace, target)
      HippoValue.BuiltinMemberFunction(targetV, member)

    case HippoExpression.Access(target, name) =>
      val targetV = eval(namespace, target)
      accessValue(targetV, name)

    case HippoExpression.Apply(target, args) =>
      val targetV = eval(namespace, target)
      val argsV = args.map(eval(namespace, _))
      applyValue(targetV, argsV)

    case HippoExpression.ApplyTactic(_, _) =>
      throw new UnsupportedOperationException("tactic application not allowed in normal mode")
  }

  // Protected because otherwise the value would have to be computed twice.
  protected def accessValue(target: HippoValue, name: HippoIdentifier): HippoValue = {
    import org.keymaerax.hippolang.BuiltinMemberFunction._

    (target, name.value) match {
      case (HippoValue.Namespace(ns), _) => ns.lookup(name)
      case (HippoValue.Tactic(_), "forward") => HippoValue.BuiltinMemberFunction(target, Forward)
      case (HippoValue.Tactic(_), "backward") => HippoValue.BuiltinMemberFunction(target, Backward)
      case (HippoValue.Tactic(_), "pure") => HippoValue.BuiltinMemberFunction(target, Pure)
      case _ => throw new UnsupportedOperationException("incorrect access")
    }
  }

  // Protected because otherwise the values would have to be computed twice.
  protected def applyValue(target: HippoValue, args: IndexedSeq[HippoValue]): HippoValue = target match {
    case HippoValue.TacticInfo(value) => applyTacticInfo(value, args)
    case HippoValue.BuiltinFunction(value) => applyBuiltinFunction(value, args)
    case HippoValue.BuiltinMemberFunction(target, value) => applyBuiltinMemberFunction(target, value, args)
    case HippoValue.Function(env, argNames, body) => applyFunction(env, argNames, body, args)
    case _ => throw new IllegalArgumentException("can only apply builtin")
  }

  private def applyTacticInfo(info: TacticInfo, args: IndexedSeq[HippoValue]): HippoValue = info
    .constructor
    .constructPositional(args.map(InterpreterPure.hippoValToTacticArg))
    .toHValue

  private def applyBuiltinFunction(target: BuiltinFunction, args: IndexedSeq[HippoValue]): HippoValue = target match {
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
      (left.asInt / right.asInt).toHValue

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

    case BuiltinFunction.Print =>
      val argStrs = args.map {
        case HippoValue.Null => "null"
        case HippoValue.Bool(value) => value.toString
        case HippoValue.Int(value) => value.toString
        case HippoValue.String(value) => value
        case HippoValue.List(values) => "<list>"
        case HippoValue.DlExpression(value) => value.toString
        case HippoValue.DlSequent(value) => value.toString
        case HippoValue.Namespace(value) => "<namespace>"
        case HippoValue.Proof(value) => "<proof>"
        case HippoValue.Tactic(value) => "<tactic>"
        case HippoValue.ProofInfo(value) => "<proof info>"
        case HippoValue.TacticInfo(value) => "<tactic info>"
        case HippoValue.BuiltinFunction(value) => s"<builtin function ${value.name}>"
        case HippoValue.BuiltinMemberFunction(target, value) => s"<builtin member function ${value.name}>"
        case HippoValue.Function(env, args, body) => s"<function>"
      }
      println(argStrs.mkString)
      HippoValue.Null

    case BuiltinFunction.Premise => throw new UnsupportedOperationException(
        s"#${BuiltinFunction.Premise.name} can only be called in the context of a graph block"
      )
  }

  private def applyBuiltinMemberFunction(
      target: HippoValue,
      value: BuiltinMemberFunction,
      args: IndexedSeq[HippoValue],
  ): HippoValue = {
    import org.keymaerax.hippolang.BuiltinMemberFunction._

    (target, value, args) match {
      case (HippoValue.Tactic(tactic: ForwardTactic), Forward, args) =>
        val premises = args.map(_.asSequent)
        ctx.forward(tactic, premises).toHValue

      // TODO Support premise hints
      case (HippoValue.Tactic(tactic: BackwardTactic), Backward, Seq(arg)) =>
        val conclusion = arg.asSequent
        ctx.backward(tactic, conclusion).toHValue

      case (HippoValue.Tactic(tactic: PureTactic), Pure, Seq()) => ctx.pure(tactic).toHValue

      case _ => throw new UnsupportedOperationException("incorrect builtin member function application")
    }
  }

  private def applyFunction(
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
}
