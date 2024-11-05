/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.run

import org.keymaerax.btactics.ToolProvider
import org.keymaerax.core.{Expression, Formula, Provable, Rule, Sequent, SubstitutionPair, URename, USubst, Variable}
import org.keymaerax.hippolochos.cache.{Cache, HippoProofFsCache, LruCache, ProvableFsCache}
import org.keymaerax.hippolochos.proof.{ExternalSource, HippoPremise, HippoProof}
import org.keymaerax.hippolochos.tools.Hash
import org.keymaerax.hippolochos.{BackwardTactic, ForwardTactic, PureTactic, Tactic}

import java.nio.file.Path

class HippoContext(val toolProvider: ToolProvider, val toolCache: Cache[Provable], val tacticCache: Cache[HippoProof]) {

  //////////////////////
  // External sources //
  //////////////////////

  private def computeQe(formula: Formula): Provable = toolCache
    .getOrCompute(Hash.start.digest(formula).build) { toolProvider.qeTool().get.qe(formula).fact.underlyingProvable }

  ////////////////////////
  // Proof constructors //
  ////////////////////////

  // These functions wrap the HippoProof constructors in a slightly nicer to use API.
  // In some occasions, they also perform slight optimizations to keep the resulting Proof smaller.
  // The Join constructor is wrapped separately later.

  def sorry(conclusion: Sequent, premises: IndexedSeq[HippoPremise]): HippoProof = HippoProof
    .External(conclusion, premises, ExternalSource.Sorry)

  def sorry(conclusion: Sequent, premises: HippoPremise*): HippoProof = sorry(conclusion, premises.toIndexedSeq)

  def qe(formula: Formula): HippoProof = {
    val provable = computeQe(formula)
    HippoProof.External(
      conclusion = provable.conclusion,
      premises = provable.subgoals.map(HippoPremise(_, mustBeProved = false)),
      source = ExternalSource.QeTool(formula),
    )
  }

  def sequent(conclusion: Sequent): HippoProof = HippoProof.Sequent(conclusion)

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

  def swap(proof: HippoProof, premise1: Int, premise2: Int): HippoProof = {
    // Even if we're not returning the result, we still construct it to run the correctness checks.
    val result = HippoProof.Swap(proof, premise1: Int, premise2: Int)
    if (premise1 == premise2) return proof
    result
  }

  def deduplicate(proof: HippoProof, premise: Int, duplicate: Int): HippoProof = HippoProof
    .Deduplicate(proof, premise, duplicate)

  def weaken(proof: HippoProof, premise: Sequent): HippoProof = HippoProof.Weaken(proof, premise)

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

  def tactic(tactic: Tactic, conclusion: Sequent, premises: IndexedSeq[Sequent]): HippoProof = tactic match {
    // We want to give the tactic as much information as possible,
    // so we try running it backwards before we try running it forwards.
    // Any PureTactic is also a BackwardTactic, so we don't need to match it separately.
    case tactic: BackwardTactic =>
      val premiseMap = premises.zipWithIndex.map(_.swap).toMap
      backward(tactic, conclusion, premiseMap)

    case tactic: ForwardTactic => forward(tactic, premises)
  }

  // These functions also cache the tactic application.

  def cachedPure(tactic: PureTactic): HippoProof = {
    val hash = Hash.start.digest("pure").digest(tactic.hash).build
    tacticCache.getOrCompute(hash) { pure(tactic) }
  }

  def cachedForward(tactic: ForwardTactic, premises: IndexedSeq[Sequent]): HippoProof = {
    val hash = Hash.start.digest("forward").digest(tactic.hash).digestSeq(premises)(_.digest(_)).build
    tacticCache.getOrCompute(hash) { forward(tactic, premises) }
  }

  def cachedBackward(tactic: BackwardTactic, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = {
    val hash = Hash
      .start
      .digest("backward")
      .digest(tactic.hash)
      .digest(conclusion)
      .digestSeq(premises.toSeq.sortBy(_._1)) { case (b, (i, p)) => b.digest(i).digest(p) }
      .build
    tacticCache.getOrCompute(hash) { backward(tactic, conclusion, premises) }
  }

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

  //////////////////////////
  // Extracting Provables //
  //////////////////////////

  private def fromExternal(external: HippoProof.External, premises: IndexedSeq[Provable]): Provable =
    external.source match {
      case ExternalSource.Sorry => ???
      case ExternalSource.QeTool(formula) =>
        val provable = external.assertConsistency(premises) { computeQe(formula) }
        HippoProof.applyPremises(provable, premises)
    }

  def provableFromLocalProof(proof: HippoProof): Provable = proof.localProvable(fromExternal)

  def provableFromGlobalProof(proof: HippoProof, premises: IndexedSeq[Provable]): Provable = proof
    .globalProvable(fromExternal, premises)

  def provableFromGlobalProof(proof: HippoProof, premises: Provable*): Provable =
    provableFromGlobalProof(proof, premises.toIndexedSeq)
}

object HippoContext {
  def withCacheDir(toolProvider: ToolProvider, cacheDir: Path): HippoContext = new HippoContext(
    toolProvider = toolProvider,
    toolCache = new ProvableFsCache(cacheDir.resolve("tool")).behind(new LruCache(1000)),
    tacticCache = new HippoProofFsCache(cacheDir.resolve("tactic")).behind(new LruCache(1000)),
  )
}
