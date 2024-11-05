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

import java.nio.file.Path

class FileInterpreter(ictx: HippoInterpreterContext, ctx: HippoContext, file: Option[Path]) {
  val exported = new MutableNamespace()

  def eval(namespace: MutableNamespace, expr: HippoExpression): HippoValue = expr match {
    case HippoExpression.Const(value) => value

    case HippoExpression.Import(pathE) =>
      // TODO Cache imported files by path (assuming a file never changes during the existence of an interpreter)
      val pathV = Path.of(eval(namespace, pathE).asString)
      val importFile =
        if (pathV.isAbsolute) pathV
        else file.getOrElse(throw new UnsupportedOperationException("code has no path")).getParent.resolve(pathV)
      val (_, importNamespace) = ictx.run(importFile)
      importNamespace.toHValue

    case HippoExpression.Declare(exports, mutable, name, value) =>
      val valueV = eval(namespace, value)
      if (exports) exported.declare(name, valueV, mutable = true)
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

    case HippoExpression.Theorem(conclusion, premises, proof) =>
      val conclusionV = eval(namespace, conclusion).asSequent
      val premisesV = premises.map(eval(namespace, _).asSequent).toIndexedSeq
      val proofV = eval(namespace, proof).asTactic
      ctx.tactic(Cached(proofV), conclusionV, premisesV).toHValue

    case HippoExpression.Sequence(exprs, returnExpr) =>
      for (expr <- exprs) eval(namespace, expr)
      returnExpr.map(eval(namespace, _)).toHValue

    case HippoExpression.Block(inner) =>
      val nestedNamespace = new MutableNamespace(Some(namespace))
      eval(nestedNamespace, inner)

    case HippoExpression.BackwardBlock(inner) => HippoValue
        .Tactic(FileInterpreterBackward.tactic(ictx = ictx, file = file, namespace = namespace.freeze, expr = inner))

    case HippoExpression.GraphBlock(inner) => HippoValue.Tactic(
        FileInterpreterGraph.tactic(ictx = ictx, ctx = ctx, file = file, namespace = namespace.freeze, expr = inner)
      )

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

    case e: HippoExpression.ApplyTactic => evalApplyTactic(namespace, e)
  }

  protected def evalApplyTactic(namespace: MutableNamespace, expr: HippoExpression.ApplyTactic): HippoValue =
    throw new UnsupportedOperationException("tactic application not supported in normal mode")

  private def accessValue(target: HippoValue, name: HippoIdentifier): HippoValue = {
    import org.keymaerax.hippolang.BuiltinMemberFunction._

    (target, name.value) match {
      case (HippoValue.Namespace(ns), _) => ns.lookup(name)
      case (HippoValue.Tactic(_), "forward") => HippoValue.BuiltinMemberFunction(target, Forward)
      case (HippoValue.Tactic(_), "backward") => HippoValue.BuiltinMemberFunction(target, Backward)
      case (HippoValue.Tactic(_), "pure") => HippoValue.BuiltinMemberFunction(target, Pure)
      case _ => throw new UnsupportedOperationException("incorrect access")
    }
  }

  private def applyValue(target: HippoValue, args: IndexedSeq[HippoValue]): HippoValue = target match {
    case HippoValue.TacticInfo(value) => applyTacticInfo(value, args)
    case HippoValue.BuiltinFunction(value) => applyBuiltinFunction(value, args)
    case HippoValue.BuiltinMemberFunction(target, value) => applyBuiltinMemberFunction(target, value, args)
    case HippoValue.Function(env, argNames, body) => applyFunction(env, argNames, body, args)
    case _ => throw new IllegalArgumentException("can only apply builtin")
  }

  private def applyTacticInfo(info: TacticInfo, args: IndexedSeq[HippoValue]): HippoValue = info
    .constructor
    .constructPositional(args.map(FileInterpreter.hippoValToTacticArg))
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

    case BuiltinFunction.Premise => applyBuiltinFunctionPremise(args)
  }

  protected def applyBuiltinFunctionPremise(args: IndexedSeq[HippoValue]): HippoValue =
    throw new UnsupportedOperationException("#premise builtin can only be called in the context of a graph block")

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

object FileInterpreter {
  private def hippoValToTacticArg(value: HippoValue): Any = value match {
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
