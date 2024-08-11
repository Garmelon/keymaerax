/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.proof

import org.keymaerax.core

sealed trait HippoProof {
  val conclusion: core.Sequent
  val premises: IndexedSeq[core.Sequent]

  def computeProvable(): core.Provable

  final def proved: Boolean = premises.isEmpty
  final def sameShapeAs(other: HippoProof): Boolean = conclusion == other.conclusion && premises == other.premises
}

object HippoProof {
  private val CQrule: HippoProof.CoreAxiomaticRule = HippoProof.CoreAxiomaticRule("CQ equation congruence")

  final case class Sorry(conclusion: core.Sequent, premises: IndexedSeq[core.Sequent]) extends HippoProof {
    override def computeProvable(): core.Provable = ???
  }

  final case class Sequent(conclusion: core.Sequent) extends HippoProof {
    val provable: core.Provable = core.Provable.startProof(conclusion)
    val premises: IndexedSeq[core.Sequent] = provable.subgoals
    override def computeProvable(): core.Provable = provable
  }

  final case class Provable(provable: core.Provable) extends HippoProof {
    val conclusion: core.Sequent = provable.conclusion
    val premises: IndexedSeq[core.Sequent] = provable.subgoals
    override def computeProvable(): core.Provable = provable
  }

  final case class CoreAxiom(name: String) extends HippoProof {
    val provable: core.Provable = core.Provable.axioms(name)
    val conclusion: core.Sequent = provable.conclusion
    val premises: IndexedSeq[core.Sequent] = provable.subgoals
    override def computeProvable(): core.Provable = provable
  }

  final case class CoreAxiomaticRule(name: String) extends HippoProof {
    val provable: core.Provable = core.Provable.rules(name)
    val conclusion: core.Sequent = provable.conclusion
    val premises: IndexedSeq[core.Sequent] = provable.subgoals
    override def computeProvable(): core.Provable = provable
  }

  final case class CoreProofRule(conclusion: core.Sequent, rule: core.Rule) extends HippoProof {
    val provable: core.Provable = core.Provable.startProof(conclusion)(rule, 0)
    val premises: IndexedSeq[core.Sequent] = provable.subgoals
    override def computeProvable(): core.Provable = provable
  }

  final case class URename(proof: HippoProof, rename: core.URename) extends HippoProof {
    val conclusion: core.Sequent = rename(proof.conclusion)
    val premises: IndexedSeq[core.Sequent] = proof.premises.map(rename(_))
    override def computeProvable(): core.Provable = proof.computeProvable()(rename)
  }

  final case class USubst(proof: HippoProof, subst: core.USubst) extends HippoProof {
    // See Provable.apply(USubst) for justification
    private val noTaboos = proof.proved || proof.sameShapeAs(CQrule)
    val conclusion: core.Sequent = if (noTaboos) subst(proof.conclusion) else subst.applyAllTaboo(proof.conclusion)
    val premises: IndexedSeq[core.Sequent] =
      if (noTaboos) proof.premises.map(subst.apply) else proof.premises.map(subst.applyAllTaboo)

    override def computeProvable(): core.Provable = proof.computeProvable()(subst)
  }

  final case class Join(proof: HippoProof, subproof: HippoProof, at: Int) extends HippoProof {
    require(0 <= at && at < proof.premises.length)

    val conclusion: core.Sequent = proof.conclusion

    val premises: IndexedSeq[core.Sequent] = {
      val before = proof.premises.take(at)
      val premise = proof.premises(at)
      val after = proof.premises.drop(at + 1)

      assert(premise == subproof.conclusion)
      before ++ subproof.premises ++ after
    }

    override def computeProvable(): core.Provable = {
      assert(premises.length == this.premises.length)
      proof.computeProvable()(subproof.computeProvable(), at)
    }
  }
}
