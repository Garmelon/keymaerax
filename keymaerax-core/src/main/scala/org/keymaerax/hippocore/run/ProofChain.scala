/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.run

import org.keymaerax.core.{Expression, Sequent, SubstitutionPair, URename, USubst, Variable}
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic}

case class ProofChain(ctx: HippoContext, proof: HippoProof) {

  /////////////////////////
  // Modifying the proof //
  /////////////////////////

  def uRename(uRename: URename): ProofChain = copy(proof = ctx.uRename(proof, uRename))

  def uRename(from: Variable, to: Variable, semantic: Boolean = false): ProofChain =
    uRename(URename(what = from, repl = to, semantic = semantic))

  def uSubst(uSubst: USubst): ProofChain = copy(proof = ctx.uSubst(proof, uSubst))

  def uSubst(substs: (Expression, Expression)*): ProofChain = uSubst(USubst(substs.map { case (from, to) =>
    SubstitutionPair(from, to)
  }))

  def swap(premise1: Int, premise2: Int): HippoProof = ctx.swap(proof, premise1, premise2)

  def deduplicate(premise: Int, duplicate: Int): HippoProof = ctx.deduplicate(proof, premise, duplicate)

  def weaken(premise: Sequent): HippoProof = ctx.weaken(proof, premise)

  /////////////////////////
  // Extending the proof //
  /////////////////////////

  // Directed versions of HippoContext.join and HippoContext.joinAll

  def joinForward(proof: HippoProof): ProofChain = joinForwardAt(0)(proof)

  def joinForwardAt(at: Int)(proof: HippoProof): ProofChain = copy(proof = ctx.joinAt(at)(proof, this.proof))

  def joinBackward(proof: HippoProof): ProofChain = joinBackwardAt(0)(proof)

  def joinBackwardAt(at: Int)(proof: HippoProof): ProofChain = copy(proof = ctx.joinAt(at)(this.proof, proof))

  def joinBackwardAll(proofs: HippoProof*): ProofChain = copy(proof = ctx.joinAll(this.proof, proofs: _*))

  // Ways to extend a proof with tactics

  def forward(tactic: ForwardTactic, otherPremises: Sequent*): ProofChain = forwardAt(0)(tactic, otherPremises: _*)

  def forwardAt(at: Int)(tactic: ForwardTactic, otherPremises: Sequent*): ProofChain = {
    val premises = (otherPremises.take(at) :+ proof.conclusion) ++ otherPremises.drop(at)
    joinForward(ctx.forward(tactic, premises: _*))
  }

  def forwardJoin(tactic: ForwardTactic, otherPremises: HippoProof*): ProofChain = {
    forwardJoinAt(0)(tactic, otherPremises: _*)
  }

  def forwardJoinAt(at: Int)(tactic: ForwardTactic, otherPremises: HippoProof*): ProofChain = {
    val premises = (otherPremises.take(at) :+ proof) ++ otherPremises.drop(at)
    copy(proof = ctx.forwardJoin(tactic, premises: _*))
  }

  def backward(tactic: BackwardTactic, premises: (Int, Sequent)*): ProofChain = backwardAt(0)(tactic, premises: _*)

  def backwardAt(at: Int)(tactic: BackwardTactic, premises: (Int, Sequent)*): ProofChain =
    copy(proof = ctx.backwardJoinAt(at)(tactic, proof, premises: _*))

  def backwardJoin(tactic: BackwardTactic, premises: (Int, HippoProof)*): ProofChain =
    backwardJoinAt(0)(tactic, premises: _*)

  def backwardJoinAt(at: Int)(tactic: BackwardTactic, premises: (Int, HippoProof)*): ProofChain = {
    val chain = backwardAt(at)(tactic, premises.map { case (i, p) => (i, p.conclusion) }: _*)
    premises.toMap.toSeq.sortBy(_._1).reverseIterator.foldLeft(chain) { case (c, (i, p)) => c.joinBackwardAt(i)(p) }
  }
}
