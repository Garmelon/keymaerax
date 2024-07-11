/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.interpret

import org.keymaerax.hippolochus.axiom.PureAxiom
import org.keymaerax.hippolochus.proof.HippoProof
import org.keymaerax.hippolochus.tactic.HippoTactic
import org.keymaerax.hippolochus.{HippoBuiltin, HippoExpression, HippoValue}

case class SequentialInterpreterState(
    interpreter: SequentialInterpreter,
    namespace: Namespace,
    proof: Option[HippoProof] = None,
) {

  def eval(expr: HippoExpression): (SequentialInterpreterState, HippoValue) = expr match {
    case e: HippoExpression.Const => (this, e.value)
    case e: HippoExpression.Theorem => evalTheorem(e)
    case e: HippoExpression.On => evalOn(e)
    case e: HippoExpression.Fork => evalFork(e)
    case HippoExpression.Done => evalDone()
    case e: HippoExpression.Lookup => evalLookup(e)
    case e: HippoExpression.Assign => evalAssign(e)
    case e: HippoExpression.Sequence => evalSequence(e)
    case e: HippoExpression.Block => evalBlock(e)
    case e: HippoExpression.Access => evalAccess(e)
    case e: HippoExpression.Apply => evalApply(e)
  }

  def prove(proof: HippoProof, expr: HippoExpression): (HippoProof, HippoValue) = {
    val (this2, value) = this.copy(proof = Some(proof)).eval(expr)

    // If this condition is violated, the interpreter has an implementation bug.
    val proof2 = this2.proof.getOrElse(throw new InternalException("eval must not leave proof context"))

    (proof2, value)
  }

  // Actual implementation of eval

  private type EvalResponse = (SequentialInterpreterState, HippoValue)

  private def currentProof() = this.proof match {
    case Some(proof) => proof
    case None => throw new NotInProofException()
  }

  private def evalTheorem(e: HippoExpression.Theorem): EvalResponse = {
    val (this2, statement) = this.eval(e.statement)
    val proof = statement match {
      case v: HippoValue.Dgl => HippoProof.fromFormula(v.value)
      case v => throw new WrongTypeException(v, HippoValue.Dgl.getClass)
    }
    val (proof2, _) = this2.prove(proof, e.proof)
    (this2, HippoValue.Axiom(PureAxiom(proof2)))
  }

  private def evalOn(e: HippoExpression.On): EvalResponse = {
    val proof = this.currentProof()

    val index = SequentialInterpreterState.resolveLabel(proof, e.label)
    val (subproof, value) = this.prove(proof.goal(index).asProof, e.proof)

    val proof2 = proof.extend(index, subproof)
    val this2 = this.copy(proof = Some(proof2))
    (this2, value)
  }

  private def evalFork(e: HippoExpression.Fork): EvalResponse = {
    val proof = this.currentProof()

    val casesWithIndex = e.cases.map(c => SequentialInterpreterState.resolveLabel(proof, c.label) -> c)
    val distinctIndices = casesWithIndex.map(_._1).distinct
    require(distinctIndices.length == casesWithIndex.length, "fork cases must be unique")

    val subproofsWithIndex = casesWithIndex.map { case (index, c) =>
      val (subproof, _) = this.prove(proof.goal(index).asProof, c.proof)
      (index, subproof)
    }

    val proof2 = proof.extendMany(subproofsWithIndex.toMap)
    val this2 = this.copy(proof = Some(proof2))
    (this2, HippoValue.Null)
  }

  private def evalDone(): EvalResponse = {
    val proof = this.currentProof()
    require(proof.isProved)
    (this, HippoValue.Null)
  }

  private def evalLookup(e: HippoExpression.Lookup): EvalResponse = {
    val value = this.namespace.lookup(e.name)
    (this, value)
  }

  private def evalAssign(e: HippoExpression.Assign): EvalResponse = {
    val (this2, value) = this.eval(e.value)
    val namespace2 = this2.namespace.assign(e.name, value)
    val this3 = this2.copy(namespace = namespace2)
    (this3, value)
  }

  private def evalSequence(e: HippoExpression.Sequence): EvalResponse = { this.eval(e.first)._1.eval(e.second) }

  private def evalBlock(e: HippoExpression.Block): EvalResponse = {
    val this2 = this.copy(namespace = this.namespace.nest())
    this2.eval(e.inner)
  }

  private def evalAccess(e: HippoExpression.Access): EvalResponse = {
    val (this2, target) = this.eval(e.target)
    target match {
      case HippoValue.Namespace(values) => (this2, Namespace(values).lookup(e.name))
      case v => throw new WrongTypeException(v)
    }
  }

  private def evalApply(e: HippoExpression.Apply): EvalResponse = {
    val (this2, target) = this.eval(e.target)
    val (this3, args) = this2.evalArgs(e.args)
    target match {
      case HippoValue.Tactic(tactic) => this3.evalApplyTactic(tactic, args)
      case HippoValue.Builtin(builtin) => this3.evalApplyBuiltin(builtin, args)
      case v => throw new WrongTypeException(v)
    }
  }

  private def evalArgs(exprs: IndexedSeq[HippoExpression]): (SequentialInterpreterState, IndexedSeq[HippoValue]) = {
    var state = this
    val values = IndexedSeq.newBuilder[HippoValue]
    for (expr <- exprs) {
      val (newState, value) = state.eval(expr)
      state = newState
      values.addOne(value)
    }
    (state, values.result())
  }

  private def evalApplyTactic(tactic: HippoTactic, args: IndexedSeq[HippoValue]): EvalResponse = {
    val proof = currentProof()

    if (proof.subgoals.length < 1) throw new TooFewGoals()
    if (proof.subgoals.length > 1) throw new TooManyGoals()
    val goal = proof.subgoals.head

    val subproof = tactic.executeWithArgs(interpreter, goal, args)

    val proof2 = proof.extend(0, subproof)
    val this2 = this.copy(proof = Some(proof2))
    (this2, HippoValue.Null)
  }

  private def evalApplyBuiltin(builtin: HippoBuiltin, args: IndexedSeq[HippoValue]): EvalResponse = {
    val value = builtin match {
      case HippoBuiltin.Not => builtinNot(args)
      case HippoBuiltin.Neg => builtinNeg(args)
      case HippoBuiltin.Mul => builtinMul(args)
      case HippoBuiltin.Div => builtinDiv(args)
      case HippoBuiltin.Add => builtinAdd(args)
      case HippoBuiltin.Sub => builtinSub(args)
      case HippoBuiltin.Gt => builtinGt(args)
      case HippoBuiltin.Gte => builtinGte(args)
      case HippoBuiltin.Lt => builtinLt(args)
      case HippoBuiltin.Lte => builtinLte(args)
      case HippoBuiltin.Eq => builtinEq(args)
      case HippoBuiltin.Neq => builtinNeq(args)
      case HippoBuiltin.And => builtinAnd(args)
      case HippoBuiltin.Or => builtinOr(args)
      case HippoBuiltin.Print => builtinPrint(args)
    }
    (this, value)
  }

  private def builtinNot(args: IndexedSeq[HippoValue]): HippoValue = {
    val arg = CriticalException.args1[HippoValue.Bool](args)
    HippoValue.Bool(!arg.value)
  }

  private def builtinNeg(args: IndexedSeq[HippoValue]): HippoValue = {
    val arg = CriticalException.args1[HippoValue.Int](args)
    HippoValue.Int(-arg.value)
  }

  private def builtinMul(args: IndexedSeq[HippoValue]): HippoValue = {
    val (arg1, arg2) = CriticalException.args2[HippoValue.Int, HippoValue.Int](args)
    HippoValue.Int(arg1.value * arg2.value)
  }

  private def builtinDiv(args: IndexedSeq[HippoValue]): HippoValue = {
    val (arg1, arg2) = CriticalException.args2[HippoValue.Int, HippoValue.Int](args)
    HippoValue.Int(arg1.value / arg2.value)
  }

  private def builtinAdd(args: IndexedSeq[HippoValue]): HippoValue = {
    val (arg1, arg2) = CriticalException.args2[HippoValue.Int, HippoValue.Int](args)
    HippoValue.Int(arg1.value + arg2.value)
  }

  private def builtinSub(args: IndexedSeq[HippoValue]): HippoValue = {
    val (arg1, arg2) = CriticalException.args2[HippoValue.Int, HippoValue.Int](args)
    HippoValue.Int(arg1.value - arg2.value)
  }

  private def builtinGt(args: IndexedSeq[HippoValue]): HippoValue = {
    val (arg1, arg2) = CriticalException.args2[HippoValue.Int, HippoValue.Int](args)
    HippoValue.Bool(arg1.value > arg2.value)
  }

  private def builtinGte(args: IndexedSeq[HippoValue]): HippoValue = {
    val (arg1, arg2) = CriticalException.args2[HippoValue.Int, HippoValue.Int](args)
    HippoValue.Bool(arg1.value >= arg2.value)
  }

  private def builtinLt(args: IndexedSeq[HippoValue]): HippoValue = {
    val (arg1, arg2) = CriticalException.args2[HippoValue.Int, HippoValue.Int](args)
    HippoValue.Bool(arg1.value < arg2.value)
  }

  private def builtinLte(args: IndexedSeq[HippoValue]): HippoValue = {
    val (arg1, arg2) = CriticalException.args2[HippoValue.Int, HippoValue.Int](args)
    HippoValue.Bool(arg1.value <= arg2.value)
  }

  private def builtinEq(args: IndexedSeq[HippoValue]): HippoValue = {
    val (arg1, arg2) = CriticalException.args2[HippoValue, HippoValue](args)
    HippoValue.Bool(arg1 == arg2)
  }

  private def builtinNeq(args: IndexedSeq[HippoValue]): HippoValue = {
    val (arg1, arg2) = CriticalException.args2[HippoValue, HippoValue](args)
    HippoValue.Bool(arg1 != arg2)
  }

  private def builtinAnd(args: IndexedSeq[HippoValue]): HippoValue = {
    val (arg1, arg2) = CriticalException.args2[HippoValue.Bool, HippoValue.Bool](args)
    HippoValue.Bool(arg1.value && arg2.value)
  }

  private def builtinOr(args: IndexedSeq[HippoValue]): HippoValue = {
    val (arg1, arg2) = CriticalException.args2[HippoValue.Bool, HippoValue.Bool](args)
    HippoValue.Bool(arg1.value || arg2.value)
  }

  private def builtinPrint(args: IndexedSeq[HippoValue]): HippoValue = {
    val strArgs = args.map {
      case HippoValue.Null => "null"
      case HippoValue.Bool(value) => value.toString
      case HippoValue.Int(value) => value.toString
      case HippoValue.String(value) => value
      case HippoValue.Builtin(value) => s"#${value.name}"
      case v => v.toString
    }
    val output = strArgs.mkString
    println(s"[hippo] $output")
    HippoValue.Null
  }
}

object SequentialInterpreterState {
  private def resolveLabel(proof: HippoProof, label: HippoValue): Int = label match {
    case HippoValue.Int(value) => proof.resolveIndex(value).getOrElse(throw new InvalidLabelException(proof, label))
    case HippoValue.String(value) => proof.resolveLabel(value).getOrElse(throw new InvalidLabelException(proof, label))
    case v => throw new WrongTypeException(v, HippoValue.Int.getClass, HippoValue.String.getClass)
  }
}
