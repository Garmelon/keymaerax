/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.run

import org.keymaerax.core.{Expression, Provable, Rule, Sequent, SubstitutionPair, URename, USubst, Variable}
import org.keymaerax.hippolochos.proof.{HippoPremise, HippoProof}
import org.keymaerax.hippolochos.{BackwardTactic, ForwardTactic, PureTactic}

class HippoContext {

  ////////////////////////
  // Proof constructors //
  ////////////////////////

  // These functions wrap the HippoProof constructors in a slightly nicer to use API.
  // In some occasions, they also perform slight optimizations to keep the resulting Proof smaller.
  // The Join constructor is wrapped separately later.

  def sorry(conclusion: Sequent, premises: IndexedSeq[Sequent]): HippoProof = HippoProof
    .Sorry(conclusion, premises.map(HippoPremise.locallySound))

  def sorry(conclusion: Sequent, premises: Sequent*): HippoProof = sorry(conclusion, premises.toIndexedSeq)

  def sequent(conclusion: Sequent): HippoProof = HippoProof.Sequent(conclusion)

  def provable(provable: Provable): HippoProof = HippoProof.Provable(provable)

  def coreAxiom(name: String): HippoProof = HippoProof.CoreAxiom(name)

  def coreAxiomaticRule(name: String): HippoProof = HippoProof.CoreAxiomaticRule(name)

  def coreProofRule(rule: Rule, conclusion: Sequent): HippoProof = HippoProof.CoreProofRule(conclusion, rule)

  def uRename(proof: HippoProof, uRename: URename): HippoProof = HippoProof.URename(proof, uRename)

  def uRename(proof: HippoProof, from: Variable, to: Variable, semantic: Boolean = false): HippoProof =
    uRename(proof, URename(what = from, repl = to, semantic = semantic))

  def uSubst(proof: HippoProof, uSubst: USubst): HippoProof = HippoProof.USubst(proof, uSubst)

  def uSubst(proof: HippoProof, substs: (Expression, Expression)*): HippoProof =
    uSubst(proof, USubst(substs.map { case (from, to) => SubstitutionPair(from, to) }))

  def uSubstGlobal(premise: Sequent, subst: USubst): HippoProof = HippoProof.GloballySoundUSubst(premise, subst)

  def uSubstGlobal(premise: Sequent, substs: (Expression, Expression)*): HippoProof =
    uSubstGlobal(premise, USubst(substs.map { case (from, to) => SubstitutionPair(from, to) }))

  /////////////////////////
  // Tactic applications //
  /////////////////////////

  // These functions wrap tactic application.

  def pure(tactic: PureTactic): HippoProof = tactic.runPure(this)

  def forward(tactic: ForwardTactic, premises: IndexedSeq[Sequent]): HippoProof = tactic.runForward(this, premises)

  def forward(tactic: ForwardTactic, premises: Sequent*): HippoProof = forward(tactic, premises.toIndexedSeq)

  def backward(tactic: BackwardTactic, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = tactic
    .runBackward(this, conclusion, premises)

  def backward(tactic: BackwardTactic, conclusion: Sequent, premises: (Int, Sequent)*): HippoProof =
    backward(tactic, conclusion, premises.toMap)

  //////////////////////
  // Combining proofs //
  //////////////////////

  def join(proof: HippoProof, subproof: HippoProof): HippoProof = joinAt(0)(proof, subproof)

  def joinAt(at: Int)(proof: HippoProof, subproof: HippoProof): HippoProof = {
    // Even if we're not returning the result, we still construct it to run the correctness checks.
    val result = HippoProof.Join(proof, subproof, at)
    if (proof.isInstanceOf[HippoProof.Sequent]) return subproof
    if (subproof.isInstanceOf[HippoProof.Sequent]) return proof
    result
  }

  def joinAll(proof: HippoProof, subproofs: HippoProof*): HippoProof = {
    require(proof.premises.length == subproofs.length)
    // Folding right to left so earlier joins don't affect the later joins' indices.
    subproofs.zipWithIndex.foldRight(proof) { case ((subproof, at), proof) => joinAt(at)(proof, subproof) }
  }

  def forwardJoin(tactic: ForwardTactic, premises: HippoProof*): HippoProof =
    joinAll(forward(tactic, premises.map(_.conclusion): _*), premises: _*)

  def backwardJoin(tactic: BackwardTactic, conclusion: HippoProof, premises: (Int, Sequent)*): HippoProof =
    backwardJoinAt(0)(tactic, conclusion, premises: _*)

  def backwardJoinAt(at: Int)(tactic: BackwardTactic, conclusion: HippoProof, premises: (Int, Sequent)*): HippoProof =
    joinAt(at)(conclusion, backward(tactic, conclusion.premises(at).sequent, premises: _*))

  ///////////////////////////
  // Starting proof chains //
  ///////////////////////////

  def chain(proof: HippoProof): ProofChain = ProofChain(this, proof)
  def chain(sequent: Sequent): ProofChain = chain(this.sequent(sequent))
}
