/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippolang.HippoConversions._
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
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
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.{BackwardTactic, ForwardTactic, PureTactic}

class InterpreterPure(ictx: HippoInterpreterContext, ctx: HippoContext) {
  def eval(namespace: MutableNamespace, expr: HippoExpression): HippoValue = expr match {
    case e: HippoExpression.Const => e.value

    case e: HippoExpression.Import => throw HlangException("import not allowed during pure evaluation", slice = e.slice)

    case e: HippoExpression.Declare =>
      for (slice <- e.exportSlice) throw HlangException("export not allowed during pure evaluation", slice = slice)
      val value = eval(namespace, e.value)
      namespace.declare(e.name, value, e.mutable)
      value

    case e: HippoExpression.Assign =>
      val value = eval(namespace, e.value)
      HlangException.at(e.slice) { namespace.assign(e.name, value) }
      value

    case e: HippoExpression.Lookup => HlangException.at(e.slice) { namespace.lookup(e.name) }

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
      val proof = eval(namespace, e.proof).asTactic
      val proven = HlangException
        .at(e.proofSlice, "while running this tactic") { ctx.tactic(Cached(proof), conclusion, premises) }

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
      HippoValue.Tactic(InterpreterBackward.tactic(ictx = ictx, namespace = namespace.freeze, expr = e.inner))

    case e: HippoExpression.GraphBlock =>
      HippoValue.Tactic(InterpreterGraph.tactic(ictx = ictx, ctx = ctx, namespace = namespace.freeze, expr = e.inner))

    case e: HippoExpression.BuiltinAccess =>
      val target = eval(namespace, e.target)
      HippoValue.BuiltinMemberFunction(target, e.member)

    case e: HippoExpression.Access =>
      val target = eval(namespace, e.target)
      accessValue(target, e.name)

    case e: HippoExpression.Apply =>
      val target = eval(namespace, e.target)
      val args = e.args.map(eval(namespace, _))
      applyValue(target, args)

    case e: HippoExpression.ApplyTactic =>
      throw HlangException("tactic application not allowed in normal mode", slice = e.slice)
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
      println(args.map(_.format).mkString)
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
