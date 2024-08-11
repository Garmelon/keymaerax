/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.proof

import org.keymaerax.core

sealed abstract class HippoProof {
  val conclusion: core.Sequent
  val premises: IndexedSeq[HippoPremise]

  final def proved: Boolean = premises.isEmpty
  final def locallySound: Boolean = premises.forall(!_.mustBeProved)
  final def sameShapeAs(other: HippoProof): Boolean = conclusion == other.conclusion && premises == other.premises

  protected def computeProvable(premises: IndexedSeq[core.Provable]): core.Provable

  final def localProvable(): core.Provable = {
    require(locallySound)
    computeProvable(premises.map(p => core.Provable.startProof(p.sequent)))
  }

  final def globalProvable(premises: IndexedSeq[core.Provable]): core.Provable = {
    require(premises.length == this.premises.length)
    for ((hp, p) <- this.premises.zip(premises)) if (hp.mustBeProved) require(p.isProved)
    computeProvable(premises)
  }

  protected final def assertConsistency(
      premises: IndexedSeq[core.Provable]
  )(provable: => core.Provable): core.Provable = {
    assert(this.premises.length == premises.length)
    assert(this.premises.zip(premises).forall { case (tp, p) => tp.sequent == p.conclusion })

    val computedProvable = provable
    assert(this.conclusion == computedProvable.conclusion)

    computedProvable
  }
}

object HippoProof {
  private val CQrule: HippoProof.CoreAxiomaticRule = HippoProof.CoreAxiomaticRule("CQ equation congruence")

  private def applyPremises(provable: core.Provable, premises: IndexedSeq[core.Provable]): core.Provable = {
    assert(provable.subgoals.length == premises.length)
    // Replace premises from right to left so the index works out.
    premises.zipWithIndex.foldRight(provable) { case ((premise, i), provable) => provable(premise, i) }
  }

  final case class Sorry(conclusion: core.Sequent, premises: IndexedSeq[HippoPremise]) extends HippoProof {
    override protected def computeProvable(premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) { ??? }
  }

  final case class Sequent(conclusion: core.Sequent) extends HippoProof {
    val provable: core.Provable = core.Provable.startProof(conclusion)
    val premises: IndexedSeq[HippoPremise] = provable.subgoals.map(HippoPremise.locallySound)

    override protected def computeProvable(premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) { applyPremises(provable, premises) }
  }

  final case class Provable(provable: core.Provable) extends HippoProof {
    val conclusion: core.Sequent = provable.conclusion
    val premises: IndexedSeq[HippoPremise] = provable.subgoals.map(HippoPremise.locallySound)

    override protected def computeProvable(premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) { applyPremises(provable, premises) }
  }

  final case class CoreAxiom(name: String) extends HippoProof {
    val provable: core.Provable = core.Provable.axioms(name)
    val conclusion: core.Sequent = provable.conclusion
    val premises: IndexedSeq[HippoPremise] = provable.subgoals.map(HippoPremise.locallySound)

    override protected def computeProvable(premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) { applyPremises(provable, premises) }
  }

  final case class CoreAxiomaticRule(name: String) extends HippoProof {
    val provable: core.Provable = core.Provable.rules(name)
    val conclusion: core.Sequent = provable.conclusion
    val premises: IndexedSeq[HippoPremise] = provable.subgoals.map(HippoPremise.locallySound)

    override protected def computeProvable(premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) { applyPremises(provable, premises) }
  }

  final case class CoreProofRule(conclusion: core.Sequent, rule: core.Rule) extends HippoProof {
    val provable: core.Provable = core.Provable.startProof(conclusion)(rule, 0)
    val premises: IndexedSeq[HippoPremise] = provable.subgoals.map(HippoPremise.locallySound)

    override protected def computeProvable(premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) { applyPremises(provable, premises) }
  }

  final case class URename(proof: HippoProof, rename: core.URename) extends HippoProof {
    require(proof.locallySound)
    val conclusion: core.Sequent = rename(proof.conclusion)
    val premises: IndexedSeq[HippoPremise] = proof.premises.map(p => p.copy(sequent = rename(p.sequent)))

    override protected def computeProvable(premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) {
        val provable = proof.localProvable()(rename)
        applyPremises(provable, premises)
      }
  }

  final case class USubst(proof: HippoProof, subst: core.USubst) extends HippoProof {
    require(proof.locallySound)
    // See Provable.apply(USubst) for justification
    private val noTaboos = proof.proved || proof.sameShapeAs(CQrule)
    val conclusion: core.Sequent = if (noTaboos) subst(proof.conclusion) else subst.applyAllTaboo(proof.conclusion)
    val premises: IndexedSeq[HippoPremise] =
      if (noTaboos) proof.premises.map(p => p.copy(sequent = subst(p.sequent)))
      else proof.premises.map(p => p.copy(sequent = subst.applyAllTaboo(p.sequent)))

    override protected def computeProvable(premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) {
        val provable = proof.localProvable()(subst)
        applyPremises(provable, premises)
      }
  }

  final case class GloballySoundUSubst(premise: core.Sequent, subst: core.USubst) extends HippoProof {
    val conclusion: core.Sequent = subst(premise)
    val premises: IndexedSeq[HippoPremise] = IndexedSeq(HippoPremise.locallyUnsound(premise))

    override protected def computeProvable(premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) { premises.head(subst) }
  }

  final case class Join(proof: HippoProof, subproof: HippoProof, at: Int) extends HippoProof {
    require(proof.premises.indices.contains(at))
    require(proof.premises(at).sequent == subproof.conclusion)

    val conclusion: core.Sequent = proof.conclusion
    val premises: IndexedSeq[HippoPremise] = {
      val before = proof.premises.take(at)
      val premise = proof.premises(at)
      val after = proof.premises.drop(at + 1)

      val subpremises =
        if (premise.mustBeProved) subproof.premises.map(_.copy(mustBeProved = true)) else subproof.premises
      before ++ subpremises ++ after
    }

    override protected def computeProvable(premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) {
        val before = premises.take(at)
        val subpremises = premises.slice(at, at + subproof.premises.length)
        val after = premises.drop(at + subproof.premises.length)

        val premise = subproof.computeProvable(subpremises)
        proof.computeProvable((before :+ premise) ++ after)
      }
  }
}
