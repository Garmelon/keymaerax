/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippocore.BackwardTactic
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.{HippoContext, ProofChain}
import org.keymaerax.hippocore.tools.HumanFormat.pluralizeN
import org.keymaerax.hippocore.tools.PremisePermuter
import org.keymaerax.hippolang.HlangConversions.*
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.{HlangException, HlangExpression, HlangIdentifier, HlangValue, PrettyPrinter}
import org.keymaerax.hippolib.primitive.Cached

import scala.collection.mutable

class InterpreterBackward(
    ictx: InterpreterContext,
    ctx: HippoContext,
    expr: HlangExpression.BackwardBlock,
    conclusion: HippoSequent,
) extends InterpreterPure(ictx, ctx) {

  override def during: String = "during backwards evaluation"

  // TODO Better error handling
  require(expr.premises.length == expr.premises.toSet.size)

  private var chain: ProofChain = ctx.chain(conclusion)
  private var goals: IndexedSeq[HlangIdentifier] = IndexedSeq(expr.conclusion)
  assert(goalsAreConsistent)

  private def goalsAreConsistent: Boolean = {
    if (chain.proof.premises.length != goals.length) return false
    if (goals.length != goals.toSet.size) return false
    true
  }

  override def eval(namespace: MutableNamespace, expr: HlangExpression): HlangValue = expr match {
    case e: HlangExpression.AssignGoal =>
      val goalIdx = goals.indexOf(e.name)
      if (goalIdx < 0) throw HlangException(s"invalid goal", e.nameSlice, "this goal is not currently open")
      val goalSequent = chain.proof.premises(goalIdx).sequent

      val (subproof, subgoals) = evalInAssignGoal(namespace, e.value, goalSequent)

      chain = chain.joinBackwardAt(goalIdx)(subproof)
      goals = goals.slice(0, goalIdx) ++ subgoals ++ goals.slice(goalIdx + 1, goals.length)
      assert(goalsAreConsistent)

      HlangValue.Null

    case e: HlangExpression.LookupGoal =>
      val goalIdx = goals.indexOf(e.name)
      if (goalIdx < 0) throw HlangException(s"invalid goal", e.slice, "this goal is not currently open")
      chain.proof.premises(goalIdx).sequent.toHValue

    case e: HlangExpression.ApplyTactic =>
      throw HlangException(s"tactic application not allowed outside goal assignment $during", slice = e.slice)

    case _ => super.eval(namespace, expr)
  }

  private def evalInAssignGoal(
      namespace: MutableNamespace,
      expr: HlangExpression,
      conclusion: HippoSequent,
  ): (HippoProof, IndexedSeq[HlangIdentifier]) = expr match {
    case e: HlangExpression.LookupGoal => (ctx.sequent(conclusion), IndexedSeq(e.name))

    case e: HlangExpression.ApplyTactic =>
      // TODO Better error handling
      // TODO Use arguments that return a plain HippoProof as hints for the tactic

      val tactic = eval(namespace, e.target).asTactic.asInstanceOf[BackwardTactic]
      var proof = HlangException.at(e.slice, "while executing this tactic") { ctx.backward(Cached(tactic), conclusion) }

      val nPremises = proof.premises.length

      val argExprs = e.args match {
        case Seq(arg: HlangExpression.Spread) => Seq.fill(nPremises)(arg.target)
        case args => args
      }

      val nArguments = argExprs.length
      if (nPremises != nArguments) throw HlangException(s"number of arguments does not match number of premises")
        .addLocation(e.argsSlice, s"the tactic has $nArguments ${pluralizeN("argument")(nArguments)}")
        .addLocation(e.slice, s"the tactic returned $nPremises ${pluralizeN("premise")(nPremises)}")

      val args = proof
        .premises
        .zip(argExprs)
        .map { case (premise, arg) => evalInAssignGoal(namespace, arg, premise.sequent) }

      // Right to left so indices don't get messed up
      proof = args
        .map { case (subproof, _) => subproof }
        .zipWithIndex
        .foldRight(proof) { case ((subproof, i), proof) => ctx.joinAt(i)(proof, subproof) }

      val goals = args.flatMap { case (_, subgoals) => subgoals }
      // This should not happen during normal operation, only if our code is buggy.
      // Hence, it's only a "require" and not a fancy HlangException.
      require(proof.premises.length == goals.length, "number of premises does not match number of goals")

      (proof, goals)

    case e =>
      // TODO Better error handling
      val proof = eval(namespace, e).asProof
      require(proof.premises.isEmpty)
      (proof, IndexedSeq.empty)
  }

  def outputProof(): HippoProof = {
    val premiseSet = expr.premises.toSet
    val goalSet = goals.toSet

    val unopenedGoals = premiseSet -- goalSet
    val unclosedGoals = goalSet -- premiseSet
    val discrepancies = mutable.Buffer.empty[String]

    if (unopenedGoals.nonEmpty) {
      val goals = unopenedGoals.toSeq.sorted.map(goal => s"- $$$goal").mkString("\n")
      discrepancies.append(s"The following goals should be open:\n$goals")
    }

    if (unclosedGoals.nonEmpty) {
      val goals = unclosedGoals
        .toSeq
        .sorted
        .map { goal =>
          val goalIdx = this.goals.indexOf(goal)
          val sequent = this.chain.proof.premises(goalIdx).sequent
          val sequentStr = PrettyPrinter.printHippoSequent(sequent)
          s"\n$$$goal:\n$sequentStr"
        }
        .mkString("\n")
      discrepancies.append(s"The following goals should be closed:\n$goals")
    }

    if (discrepancies.nonEmpty)
      throw HlangException(discrepancies.mkString("\n\n"), slice = expr.slice, label = "while executing this tactic")

    // Should always be true thanks to the discrepancy check above
    require(premiseSet == goalSet)

    val indexByGoal = expr.premises.zipWithIndex.toMap
    val targetIndices = goals.map(indexByGoal)
    PremisePermuter.permute(ctx, chain.proof, targetIndices)
  }
}

object InterpreterBackward {
  def tactic(
      ictx: InterpreterContext,
      namespace: ImmutableNamespace,
      expr: HlangExpression.BackwardBlock,
  ): InterpreterBackwardTactic = InterpreterBackwardTactic(ictx, namespace, expr)
}
