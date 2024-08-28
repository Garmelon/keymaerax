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

  protected def computeProvable(
      fromExternal: HippoProof.FromExternal,
      premises: IndexedSeq[core.Provable],
  ): core.Provable

  final def localProvable(fromExternal: HippoProof.FromExternal): core.Provable = {
    require(locallySound)
    computeProvable(fromExternal, premises.map(p => core.Provable.startProof(p.sequent)))
  }

  final def globalProvable(
      fromExternal: HippoProof.FromExternal,
      premises: IndexedSeq[core.Provable],
  ): core.Provable = {
    require(premises.length == this.premises.length)
    for ((hp, p) <- this.premises.zip(premises)) if (hp.mustBeProved) require(p.isProved)
    computeProvable(fromExternal, premises)
  }

  final def assertConsistency(premises: IndexedSeq[core.Provable])(provable: => core.Provable): core.Provable = {
    assert(this.premises.length == premises.length)
    assert(this.premises.zip(premises).forall { case (tp, p) => tp.sequent == p.conclusion })

    val computedProvable = provable
    assert(this.conclusion == computedProvable.conclusion)

    computedProvable
  }
}

object HippoProof {
  private val CQrule: HippoProof.CoreAxiomaticRule = HippoProof.CoreAxiomaticRule("CQ equation congruence")

  def applyPremises(provable: core.Provable, premises: IndexedSeq[core.Provable]): core.Provable = {
    assert(provable.subgoals.length == premises.length)
    // Replace premises from right to left so the index works out.
    premises.zipWithIndex.foldRight(provable) { case ((premise, i), provable) => provable(premise, i) }
  }

  private type FromExternal = (External, IndexedSeq[core.Provable]) => core.Provable

  final case class External(conclusion: core.Sequent, premises: IndexedSeq[HippoPremise], source: ExternalSource)
      extends HippoProof {
    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = fromExternal(this, premises)
  }

  final case class Sequent(conclusion: core.Sequent) extends HippoProof {
    val provable: core.Provable = core.Provable.startProof(conclusion)
    val premises: IndexedSeq[HippoPremise] = provable.subgoals.map(HippoPremise.locallySound)

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) { applyPremises(provable, premises) }
  }

  final case class CoreAxiom(name: String) extends HippoProof {
    val provable: core.Provable = core.Provable.axioms(name)
    val conclusion: core.Sequent = provable.conclusion
    val premises: IndexedSeq[HippoPremise] = provable.subgoals.map(HippoPremise.locallySound)

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) { applyPremises(provable, premises) }
  }

  final case class CoreAxiomaticRule(name: String) extends HippoProof {
    val provable: core.Provable = core.Provable.rules(name)
    val conclusion: core.Sequent = provable.conclusion
    val premises: IndexedSeq[HippoPremise] = provable.subgoals.map(HippoPremise.locallySound)

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) { applyPremises(provable, premises) }
  }

  final case class CoreProofRule(conclusion: core.Sequent, rule: core.Rule) extends HippoProof {
    val provable: core.Provable = core.Provable.startProof(conclusion)(rule, 0)
    val premises: IndexedSeq[HippoPremise] = provable.subgoals.map(HippoPremise.locallySound)

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) { applyPremises(provable, premises) }
  }

  final case class URename(proof: HippoProof, rename: core.URename) extends HippoProof {
    require(proof.locallySound)
    val conclusion: core.Sequent = rename(proof.conclusion)
    val premises: IndexedSeq[HippoPremise] = proof.premises.map(p => p.copy(sequent = rename(p.sequent)))

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) {
      val provable = proof.localProvable(fromExternal)(rename)
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

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) {
      val provable = proof.localProvable(fromExternal)(subst)
      applyPremises(provable, premises)
    }
  }

  final case class GloballySoundUSubst(premise: core.Sequent, subst: core.USubst) extends HippoProof {
    val conclusion: core.Sequent = subst(premise)
    val premises: IndexedSeq[HippoPremise] = IndexedSeq(HippoPremise.locallyUnsound(premise))

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) { premises.head(subst) }
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

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) {
      val before = premises.take(at)
      val subpremises = premises.slice(at, at + subproof.premises.length)
      val after = premises.drop(at + subproof.premises.length)

      val premise = subproof.computeProvable(fromExternal, subpremises)
      proof.computeProvable(fromExternal, (before :+ premise) ++ after)
    }
  }

  final case class Swap(proof: HippoProof, premise1: Int, premise2: Int) extends HippoProof {
    require(proof.premises.indices.contains(premise1))
    require(proof.premises.indices.contains(premise2))

    val conclusion: core.Sequent = proof.conclusion
    val premises: IndexedSeq[HippoPremise] = for (i <- proof.premises.indices)
      yield proof.premises(if (i == premise1) premise2 else if (i == premise2) premise1 else i)

    override def computeProvable(fromExternal: FromExternal, premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) {
        val swappedPremises = for (i <- premises.indices)
          yield premises(if (i == premise1) premise2 else if (i == premise2) premise1 else i)
        proof.computeProvable(fromExternal, swappedPremises).swap(premise1, premise2)
      }
  }

  final case class Deduplicate(proof: HippoProof, premise: Int, duplicate: Int) extends HippoProof {
    require(proof.premises.indices.contains(premise))
    require(proof.premises.indices.contains(duplicate))
    require(proof.premises(premise).sequent == proof.premises(duplicate).sequent)

    val conclusion: core.Sequent = proof.conclusion
    val premises: IndexedSeq[HippoPremise] = {
      val actualPremise = proof.premises(premise)
      val duplicatePremise = proof.premises(premise)
      val newPremise = actualPremise.copy(mustBeProved = actualPremise.mustBeProved || duplicatePremise.mustBeProved)
      val updatedPremises = proof.premises.updated(premise, newPremise)
      updatedPremises.take(duplicate) ++ updatedPremises.drop(duplicate + 1)
    }

    override def computeProvable(fromExternal: FromExternal, premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) {
        val duplicatedPremise = if (premise < duplicate) premises(premise) else premises(premise - 1)
        val duplicatedPremises = (premises.take(duplicate) :+ duplicatedPremise) ++ premises.drop(duplicate)
        proof.computeProvable(fromExternal, duplicatedPremises).deduplicate(premise, duplicate)
      }
  }

  final case class Weaken(proof: HippoProof, premise: core.Sequent) extends HippoProof {
    val conclusion: core.Sequent = proof.conclusion
    val premises: IndexedSeq[HippoPremise] = proof.premises :+ HippoPremise(sequent = premise, mustBeProved = false)

    override def computeProvable(fromExternal: FromExternal, premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) { proof.computeProvable(fromExternal, premises.dropRight(1)).weaken(premise) }
  }
}
