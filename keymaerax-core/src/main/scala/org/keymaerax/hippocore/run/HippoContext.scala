/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.run

import org.keymaerax.btactics.ToolProvider
import org.keymaerax.core.{Expression, Formula, Provable, Rule, SubstitutionPair, URename, USubst, Variable}
import org.keymaerax.hippocore.cache.{Cache, HippoProofFsCache, LruCache, ProvableFsCache}
import org.keymaerax.hippocore.definitions.{Definitions, Name}
import org.keymaerax.hippocore.proof.{ExternalSource, HippoPremise, HippoProof, HippoSequent}
import org.keymaerax.hippocore.tools.Hasher
import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic, HippoException, PureTactic, Tactic}
import org.keymaerax.hippolib.belle.Belle

import java.nio.file.Path

class HippoContext(
    val toolProvider: ToolProvider,
    val toolCache: Cache[Provable],
    val tacticCache: Cache[HippoProof],
    val proofCache: Cache[Provable],
) {

  //////////////////////
  // External sources //
  //////////////////////

  private def computeQe(formula: Formula): Provable = toolCache
    .getOrCompute(Hasher().digest(formula).hash) { toolProvider.qeTool().get.qe(formula).fact.underlyingProvable }

  ////////////////////////
  // Proof constructors //
  ////////////////////////

  // These functions wrap the HippoProof constructors in a slightly nicer to use API.
  // In some occasions, they also perform slight optimizations to keep the resulting Proof smaller.
  // The Join constructor is wrapped separately later.

  def sorry(conclusion: HippoSequent, premises: IndexedSeq[HippoPremise]): HippoProof = HippoProof
    .External(conclusion, premises, ExternalSource.Sorry)

  def sorry(conclusion: HippoSequent, premises: HippoPremise*): HippoProof = sorry(conclusion, premises.toIndexedSeq)

  def qe(formula: Formula, defs: Definitions = Definitions.empty): HippoProof = {
    val provable = computeQe(formula)
    HippoProof.External(
      conclusion = HippoSequent(provable.conclusion, defs),
      premises = provable.subgoals.map(HippoSequent(_, defs)).map(HippoPremise.locallySound),
      source = ExternalSource.QeTool(formula, defs),
    )
  }

  def belle(provable: Provable, defs: Definitions = Definitions.empty): HippoProof = HippoProof.External(
    conclusion = HippoSequent(provable.conclusion, defs),
    premises = provable.subgoals.map(HippoSequent(_, defs)).map(HippoPremise.locallySound),
    source = ExternalSource.Bellerophon(provable, defs),
  )

  def sequent(conclusion: HippoSequent): HippoProof = HippoProof.Sequent(conclusion)

  def coreAxiom(name: String): HippoProof = HippoProof.CoreAxiom(name)

  def coreAxiomaticRule(name: String): HippoProof = HippoProof.CoreAxiomaticRule(name)

  def coreProofRule(rule: Rule, conclusion: HippoSequent, defs: Definitions = Definitions.empty): HippoProof =
    HippoProof.CoreProofRule(conclusion, rule, defs)

  def uRename(proof: HippoProof, uRename: URename): HippoProof = HippoProof.URename(proof, uRename)

  def uRename(proof: HippoProof, from: Variable, to: Variable, semantic: Boolean = false): HippoProof =
    uRename(proof, URename(what = from, repl = to, semantic = semantic))

  def uSubst(proof: HippoProof, uSubst: USubst): HippoProof = HippoProof.USubst(proof, uSubst)

  def uSubst(proof: HippoProof, substs: (Expression, Expression)*): HippoProof =
    uSubst(proof, USubst(substs.map { case (from, to) => SubstitutionPair(from, to) }))

  def uSubstGlobal(premise: HippoSequent, subst: USubst): HippoProof = HippoProof.GloballySoundUSubst(premise, subst)

  def uSubstGlobal(premise: HippoSequent, substs: (Expression, Expression)*): HippoProof =
    uSubstGlobal(premise, USubst(substs.map { case (from, to) => SubstitutionPair(from, to) }))

  def expand(conclusion: HippoSequent, name: Name): HippoProof = HippoProof.Expand(conclusion, name)
  def expand(conclusion: HippoSequent, name: String, index: Option[Int] = None): HippoProof =
    expand(conclusion, Name(name, index))

  def expandAll(conclusion: HippoSequent): HippoProof = HippoProof.ExpandAll(conclusion)

  def swap(proof: HippoProof, premise1: Int, premise2: Int): HippoProof = {
    // Even if we're not returning the result, we still construct it to run the correctness checks.
    val result = HippoProof.Swap(proof, premise1: Int, premise2: Int)
    if (premise1 == premise2) return proof
    result
  }

  def deduplicate(proof: HippoProof, premise: Int, duplicate: Int): HippoProof = HippoProof
    .Deduplicate(proof, premise, duplicate)

  def weaken(proof: HippoProof, premise: HippoSequent): HippoProof = HippoProof.Weaken(proof, premise)

  /////////////////////////
  // Tactic applications //
  /////////////////////////

  // These functions wrap tactic application.

  def pure(tactic: PureTactic): HippoProof = tactic.runPure(this)

  def forward(tactic: ForwardTactic, premises: IndexedSeq[HippoSequent]): HippoProof = tactic.runForward(this, premises)

  def forward(tactic: ForwardTactic, premises: HippoSequent*): HippoProof = forward(tactic, premises.toIndexedSeq)

  def backward(tactic: BackwardTactic, conclusion: HippoSequent, premises: Map[Int, HippoSequent]): HippoProof = tactic
    .runBackward(this, conclusion, premises)

  def backward(tactic: BackwardTactic, conclusion: HippoSequent, premises: (Int, HippoSequent)*): HippoProof =
    backward(tactic, conclusion, premises.toMap)

  def tactic(tactic: Tactic, conclusion: HippoSequent, premises: IndexedSeq[HippoSequent]): HippoProof = tactic match {
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
    val hash = Hasher().digest("pure").digest(tactic.hash).hash
//    val proof = tacticCache.getOrCompute(hash) { pure(tactic) }
    val proof = pure(tactic)
    HippoProof.External(proof.conclusion, proof.premises, ExternalSource.Cache(hash))
  }

  def cachedForward(tactic: ForwardTactic, premises: IndexedSeq[HippoSequent]): HippoProof = {
    val hash = Hasher().digest("forward").digest(tactic.hash).digestSeqWith(premises)(_.digest(_)).hash
//    val proof = tacticCache.getOrCompute(hash) { forward(tactic, premises) }
    val proof = forward(tactic, premises)
    HippoProof.External(proof.conclusion, proof.premises, ExternalSource.Cache(hash))
  }

  def cachedBackward(tactic: BackwardTactic, conclusion: HippoSequent, premises: Map[Int, HippoSequent]): HippoProof = {
    val hash = Hasher()
      .digest("backward")
      .digest(tactic.hash)
      .digest(conclusion)
      .digestSeqWith(premises.toSeq.sortBy(_._1)) { case (b, (i, p)) => b.digest(i).digest(p) }
      .hash
//    val proof = tacticCache.getOrCompute(hash) { backward(tactic, conclusion, premises) }
//    val proof = backward(tactic, conclusion, premises)
    val proof = tactic match {
      case Belle("QE", _*) => tacticCache.getOrCompute(hash) { backward(tactic, conclusion, premises) }
      case _ => backward(tactic, conclusion, premises)
    }
    HippoProof.External(proof.conclusion, proof.premises, ExternalSource.Cache(hash))
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
    joinAll(forward(tactic, premises.map(_.conclusion)*), premises*)

  def backwardJoin(tactic: BackwardTactic, conclusion: HippoProof, premises: (Int, HippoSequent)*): HippoProof =
    backwardJoinAt(0)(tactic, conclusion, premises*)

  def backwardJoinAt(
      at: Int
  )(tactic: BackwardTactic, conclusion: HippoProof, premises: (Int, HippoSequent)*): HippoProof =
    joinAt(at)(conclusion, backward(tactic, conclusion.premises(at).sequent, premises*))

  ///////////////////////////
  // Starting proof chains //
  ///////////////////////////

  def chain(proof: HippoProof): ProofChain = ProofChain(this, proof)
  def chain(sequent: HippoSequent): ProofChain = chain(this.sequent(sequent))

  //////////////////////////
  // Extracting Provables //
  //////////////////////////

  private def fromExternal(external: HippoProof.External, premises: IndexedSeq[Provable]): Provable =
    external.source match {
      case ExternalSource.Sorry => HippoException.fail("Proof uses sorry")
      case ExternalSource.QeTool(formula, defs) =>
        HippoProof.applyPremises(defs.expandAll(computeQe(formula)), premises)
      case ExternalSource.Bellerophon(provable, defs) => HippoProof.applyPremises(defs.expandAll(provable), premises)
      case ExternalSource.Cache(hash) =>
        val proof = tacticCache.get(hash).get // TODO Throw some more appropriate exception?
        provableFromGlobalProof(proof, premises)
    }

  def provableFromLocalProof(proof: HippoProof): Provable = {
    val hash = Hasher().digest("local").digest(proof).hash
    proofCache.getOrCompute(hash) { proof.localProvable(fromExternal) }
  }

  def provableFromGlobalProof(proof: HippoProof, premises: IndexedSeq[Provable]): Provable = {
    val hash = Hasher().digest("global").digest(proof).digestSeqWith(premises)(_.digest(_)).hash
    proofCache.getOrCompute(hash) { proof.globalProvable(fromExternal, premises) }
  }

  def provableFromGlobalProof(proof: HippoProof, premises: Provable*): Provable =
    provableFromGlobalProof(proof, premises.toIndexedSeq)
}

object HippoContext {
  def withCacheDir(toolProvider: ToolProvider, cacheDir: Path): HippoContext = new HippoContext(
    toolProvider = toolProvider,
    toolCache = new ProvableFsCache(cacheDir.resolve("tool")).behind(new LruCache(1000)),
    tacticCache = new HippoProofFsCache(cacheDir.resolve("tactic")).behind(new LruCache(1000)),
    proofCache = new ProvableFsCache(cacheDir.resolve("proof")).behind(new LruCache(1000)),
  )
}
