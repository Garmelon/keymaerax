/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.proof

import org.keymaerax.core.{Formula, Provable, Sequent}
import org.keymaerax.hippolochus.axiom.HippoAxiom
import org.keymaerax.hippolochus.rule.HippoRule

case class HippoProof private (conclusion: Sequent, subgoals: IndexedSeq[HippoGoal], evidence: HippoEvidence) {
  { // Consistency checks
    val labelNames = subgoals.flatMap(_.label)
    require(labelNames.length == labelNames.distinct.length, "label names must be unique")
  }

  def isProved: Boolean = subgoals.isEmpty

  def computeProvable: Provable = {
    val provable = evidence.computeProvable
    require(provable.conclusion == conclusion, "evidence must not change conclusion")
    require(provable.subgoals == subgoals.map(_.sequent), "evidence must not change subgoals")
    provable
  }

  def resolveIndex(index: Int): Option[Int] = if (0 <= index && index < subgoals.length) Some(index) else None

  def resolveLabel(name: String): Option[Int] = {
    val index = subgoals.indexWhere(_.label.contains(name))
    if (index >= 0) Some(index) else None
  }

  def goal(index: Int): HippoGoal = subgoals(index)

  def sorry(): HippoProof = copy(subgoals = IndexedSeq(), evidence = HippoEvidence.Sorry())

  def label(index: Int, label: Option[String]): HippoProof = this
    .copy(subgoals = subgoals.zipWithIndex.map { case (goal, i) => if (i == index) goal.copy(label = label) else goal })

  def extend(index: Int, extension: HippoProof): HippoProof = {
    // Sanity checks first
    val evidence = HippoEvidence.Extended(this, index, extension)

    copy(
      subgoals = subgoals.slice(0, index) ++ extension.subgoals ++ subgoals.slice(index + 1, subgoals.size),
      evidence = evidence,
    )
  }

  def extendMany(extensions: Map[Int, HippoProof]): HippoProof = {
    // Sanity checks first
    val evidence = HippoEvidence.ExtendedMany(this, extensions)

    copy(
      subgoals =
        subgoals.zipWithIndex.flatMap { case (goal, i) => extensions.get(i).map(_.subgoals).getOrElse(Seq(goal)) },
      evidence = evidence,
    )
  }
}

object HippoProof {
  def fromGoal(goal: HippoGoal): HippoProof =
    HippoProof(conclusion = goal.sequent, subgoals = IndexedSeq(goal), evidence = HippoEvidence.Trivial(goal.sequent))

  def fromSequent(goal: Sequent, label: Option[String] = None): HippoProof = HippoProof
    .fromGoal(HippoGoal(sequent = goal, label = label))

  def fromFormula(goal: Formula, label: Option[String] = None): HippoProof = {
    val sequent = Sequent(ante = IndexedSeq(), succ = IndexedSeq(goal))
    HippoProof.fromSequent(goal = sequent, label = label)
  }

  def fromProvable(provable: Provable): HippoProof = HippoProof(
    conclusion = provable.conclusion,
    subgoals = provable.subgoals.map(HippoGoal(_)),
    evidence = HippoEvidence.Provable(provable),
  )

  def fromAxiom(axiom: HippoAxiom): HippoProof = axiom.proof.copy(evidence = HippoEvidence.Axiom(axiom))

  def fromRule(rule: HippoRule): HippoProof = rule.proof.copy(evidence = HippoEvidence.Rule(rule))
}
