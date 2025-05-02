/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.proof

import org.keymaerax.core
import org.keymaerax.hippocore.tools.{Hashable, Hasher}

import scala.collection.mutable

sealed abstract class HippoProof extends Hashable {
  val conclusion: HippoSequent
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
    computeProvable(fromExternal, premises.map(p => core.Provable.startProof(p.sequent.sequentExpanded)))
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
    assert(this.premises.zip(premises).forall { case (tp, p) => tp.sequent.sequentExpanded == p.conclusion })

    val computedProvable = provable
    assert(this.conclusion.sequentExpanded == computedProvable.conclusion)

    computedProvable
  }
}

object HippoProof {
  private val CQrule: HippoProof.CoreAxiomaticRule = HippoProof.CoreAxiomaticRule("CQ equation congruence")

  /**
   * Extend a proof with a subproof at a specified premise while keeping the subproof's premises together and in order.
   *
   * Extending
   * {{{
   *    S1 ... Si ... Sn
   *   ------------------
   *           R
   * }}}
   * with
   * {{{
   *    T1 T2 ... Tn
   *   --------------
   *         Si
   * }}}
   * results in
   * {{{
   *    S1 ...  T1 T2 ... Tn  ... Sn
   *   ------------------------------
   *                 R
   * }}}
   *
   * The core's method for joining Provables does this slightly differently: If the subproof has more than one premise,
   * the first premise is replaced in-place and all remaining premises are appended at the end.
   *
   * In other words, the above example would result in
   * {{{
   *    S1 ... T1 ... Sn    T2 ... Tn
   *   -------------------------------
   *                  R
   * }}}
   *
   * In order to fix this, we use [[core.Provable.swap]] to reorder the premises after joining. Note that this
   * reordering is a permutation consisting of cycles. We need len(cycle)-1 swaps per cycle, so to reorder n elements,
   * we need n-amount_of_cycles swaps, no matter what we do. The algorithm below is essentially just resolving the
   * cycles one-by-one.
   */
  def applyPremise(proof: core.Provable, subproof: core.Provable, at: Int): core.Provable = {
    require(proof.subgoals.indices.contains(at))
    require(proof.subgoals(at) == subproof.conclusion)

    var result = proof(subproof, at)
    if (subproof.subgoals.length <= 1) return result

    val l = proof.subgoals.length
    val sl = subproof.subgoals.length
    // We now need to swap the ranges [at+1, l) and [l, l+sl-1) using only
    // element-wise swaps. To keep track of our current state in-between swaps,
    // we'll use a list with one target index per premise in the result
    // provable. Our goal is that this list looks like [0, 1, ..., l+sl-2].
    // (The -2 comes from the fact that we're indexing the list starting at 0
    // and that we're removing the premise we're replacing.)
    val indices = ((0 until at + 1) ++ (l until l + sl - 1) ++ (at + 1 until l)).to(mutable.IndexedSeq)
    require(indices.length == result.subgoals.length)

    // Now we can proceed through the list of premises. When we hit an element
    // that is not at its target index, we swap it there and re-inspect our
    // current position. We're essentially swapping our way through the cycle
    // the original element was on, moving every single of its elements to the
    // correct position.
    //
    // Invariant: All elements to the left of our current position are already
    // in their correct position. From this follows that all elements starting
    // at our current position are greater or equal to our current position.
    for (i <- result.subgoals.indices) while (indices(i) != i) {
      val j = indices(i)
      // Swap elements at i and indices(i)
      indices(i) = indices(j)
      indices(j) = j
      result = result.swap(i, j)
    }

    result
  }

  def applyPremises(provable: core.Provable, premises: IndexedSeq[core.Provable]): core.Provable = {
    assert(provable.subgoals.length == premises.length)
    // Replace premises from right to left so the index works out.
    premises.zipWithIndex.foldRight(provable) { case ((premise, i), provable) => applyPremise(provable, premise, i) }
  }

  private type FromExternal = (External, IndexedSeq[core.Provable]) => core.Provable

  final case class External(conclusion: HippoSequent, premises: IndexedSeq[HippoPremise], source: ExternalSource)
      extends HippoProof {

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) { fromExternal(this, premises) }

    override def digestInto(hasher: Hasher): Unit = hasher
      .digest[this.type]
      .digest(conclusion)
      .digestSeq(premises)
      .digest(source)
  }

  final case class Sequent(conclusion: HippoSequent) extends HippoProof {
    val provable: core.Provable = core.Provable.startProof(conclusion.sequentExpanded)
    val premises: IndexedSeq[HippoPremise] = IndexedSeq(HippoPremise.locallySound(conclusion))

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) { applyPremises(conclusion.defs.expandAll(provable), premises) }

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(conclusion)
  }

  final case class CoreAxiom(name: String) extends HippoProof {
    val provable: core.Provable = core.Provable.axioms(name)
    val conclusion: HippoSequent = HippoSequent(provable.conclusion)
    val premises: IndexedSeq[HippoPremise] = provable.subgoals.map(HippoSequent(_)).map(HippoPremise.locallySound)

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) { applyPremises(provable, premises) }

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(name)
  }

  final case class CoreAxiomaticRule(name: String) extends HippoProof {
    val provable: core.Provable = core.Provable.rules(name)
    val conclusion: HippoSequent = HippoSequent(provable.conclusion)
    val premises: IndexedSeq[HippoPremise] = provable.subgoals.map(HippoSequent(_)).map(HippoPremise.locallySound)

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) { applyPremises(provable, premises) }

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(name)
  }

  final case class CoreProofRule(conclusion: HippoSequent, rule: core.Rule) extends HippoProof {
    val provable: core.Provable = core.Provable.startProof(conclusion.sequent)(rule, 0)

    val premises: IndexedSeq[HippoPremise] = provable
      .subgoals
      .map(HippoSequent(_, conclusion.defs))
      .map(HippoPremise.locallySound)

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) { applyPremises(conclusion.defs.expandAll(provable), premises) }

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(conclusion).digest(rule)
  }

  final case class URename(proof: HippoProof, rename: core.URename) extends HippoProof {
    require(proof.locallySound)

    val conclusion: HippoSequent = proof.conclusion.applyRename(rename)
    val premises: IndexedSeq[HippoPremise] = proof.premises.map(_.mapSequent(_.applyRename(rename)))

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) {
      val provable = proof.localProvable(fromExternal)(rename)
      applyPremises(provable, premises)
    }

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(proof).digest(rename)
  }

  final case class USubst(proof: HippoProof, subst: core.USubst) extends HippoProof {
    require(proof.locallySound)

    // See Provable.apply(USubst) for justification
    private val noTaboos = proof.proved || proof.sameShapeAs(CQrule)
    val conclusion: HippoSequent =
      if (noTaboos) proof.conclusion.applySubst(subst) else proof.conclusion.applySubstAllTaboo(subst)
    val premises: IndexedSeq[HippoPremise] =
      if (noTaboos) proof.premises.map(_.mapSequent(_.applySubst(subst)))
      else proof.premises.map(_.mapSequent(_.applySubstAllTaboo(subst)))

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) {
      val provable = proof.localProvable(fromExternal)(subst)
      applyPremises(provable, premises)
    }

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(proof).digest(subst)
  }

  final case class GloballySoundUSubst(premise: HippoSequent, subst: core.USubst) extends HippoProof {
    val conclusion: HippoSequent = premise.applySubst(subst)
    val premises: IndexedSeq[HippoPremise] = IndexedSeq(HippoPremise.locallyUnsound(premise))

    override protected def computeProvable(
        fromExternal: FromExternal,
        premises: IndexedSeq[core.Provable],
    ): core.Provable = assertConsistency(premises) { premises.head(subst) }

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(premise).digest(subst)
  }

  final case class Join(proof: HippoProof, subproof: HippoProof, at: Int) extends HippoProof {
    require(proof.premises.indices.contains(at))
    require(proof.premises(at).sequent == subproof.conclusion)

    val conclusion: HippoSequent = proof.conclusion
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

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(proof).digest(subproof).digest(at)
  }

  final case class Swap(proof: HippoProof, premise1: Int, premise2: Int) extends HippoProof {
    require(proof.premises.indices.contains(premise1))
    require(proof.premises.indices.contains(premise2))

    val conclusion: HippoSequent = proof.conclusion
    val premises: IndexedSeq[HippoPremise] = for (i <- proof.premises.indices)
      yield proof.premises(if (i == premise1) premise2 else if (i == premise2) premise1 else i)

    override def computeProvable(fromExternal: FromExternal, premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) {
        val swappedPremises = for (i <- premises.indices)
          yield premises(if (i == premise1) premise2 else if (i == premise2) premise1 else i)
        proof.computeProvable(fromExternal, swappedPremises).swap(premise1, premise2)
      }

    override def digestInto(hasher: Hasher): Unit = hasher
      .digest[this.type]
      .digest(proof)
      .digest(premise1)
      .digest(premise2)
  }

  final case class Deduplicate(proof: HippoProof, premise: Int, duplicate: Int) extends HippoProof {
    require(proof.premises.indices.contains(premise))
    require(proof.premises.indices.contains(duplicate))
    require(proof.premises(premise).sequent == proof.premises(duplicate).sequent)

    val conclusion: HippoSequent = proof.conclusion
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

    override def digestInto(hasher: Hasher): Unit = hasher
      .digest[this.type]
      .digest(proof)
      .digest(premise)
      .digest(duplicate)
  }

  final case class Weaken(proof: HippoProof, premise: HippoSequent) extends HippoProof {
    val conclusion: HippoSequent = proof.conclusion
    val premises: IndexedSeq[HippoPremise] = proof.premises :+ HippoPremise(sequent = premise, mustBeProved = false)

    override def computeProvable(fromExternal: FromExternal, premises: IndexedSeq[core.Provable]): core.Provable =
      assertConsistency(premises) {
        proof.computeProvable(fromExternal, premises.dropRight(1)).weaken(premise.sequentExpanded)
      }

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(proof).digest(premise)
  }
}
