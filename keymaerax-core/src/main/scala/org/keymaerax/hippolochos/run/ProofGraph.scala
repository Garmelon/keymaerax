/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.run

import org.keymaerax.core.Sequent
import org.keymaerax.hippolochos.proof.HippoProof

import scala.collection.mutable

/**
 * A directed acyclic graph of [[HippoProof]]s that can be reduced to a single [[HippoProof]].
 *
 * The graph has zero or more input premises, which can be initialized using [[premise]]. From these premises,
 * conclusions can be constructed using [[step]].
 *
 * A [[HippoProof]] of any graph node can be retrieved using [[proof]]. The proof always uses all the graph's premises,
 * in the order they were added.
 *
 * =Example=
 *
 * This example code builds a graph and then retrieves a proof from it. The variables `sequent1` to `sequent4` are
 * [[Sequent]]s; `proof1` and `proof2` are [[HippoProof]]s. Premise 2 is unused in the graph, and premise 3 is used
 * twice.
 *
 * {{{
 *   val graph = new ProofGraph
 *   val prem1 = graph.premise(sequent1)
 *   val _ = graph.premise(sequent2)
 *   val prem3 = graph.premise(sequent3)
 *   val prem4 = graph.premise(sequent4)
 *   val step1 = graph.step(proof1, prem3, prem4)
 *   val step2 = graph.step(proof2, prem1, prem3, step1)
 *   val proof = graph.proof(step2)
 * }}}
 *
 * ASCII representation of the graph:
 *
 * {{{
 *   premises:    1 2 3  4
 *                |  / \ |
 *   step 1:      | |  ===
 *                | |   |
 *   step 2:      =======
 *                   |
 *   conclusion:     x
 * }}}
 *
 * The resulting proof will include all premises in order, and each premise exactly once:
 *
 * {{{
 *    1 2 3 4
 *   ---------
 *       x
 * }}}
 */
class ProofGraph(ctx: HippoContext) {

  /** A [[Var]] represents a sequent in the [[ProofGraph]] graph. */
  //
  // An input premise is represented by a var with negative id (id = -i - 1).
  // The conclusions of a step is represented by a var whose id is the index of the step.
  //
  // As this is an inner class, the compiler will ensure that vars used in this graph must also come from this graph.
  // In particular, users can't pass the step function an invalid var, for example a premise from a different graph.
  // This mechanism also ensures steps are added in topological order.
  final class Var private[ProofGraph] (private[ProofGraph] val id: Int)

  /** A [[HippoProof]] that tracks its premise ids. */
  private case class StepProof(proof: HippoProof, premises: IndexedSeq[Int]) {
    def join(subproof: StepProof, at: Int): StepProof = StepProof(
      proof = ctx.joinAt(at)(proof, subproof.proof),
      premises = premises.take(at) ++ subproof.premises ++ premises.drop(at + 1),
    )

    def swap(at1: Int, at2: Int): StepProof = StepProof(
      proof = ctx.swap(proof, at1, at2),
      premises = for (i <- premises.indices) yield premises(if (i == at1) at2 else if (i == at2) at1 else i),
    )

    def deduplicate(premise: Int, duplicate: Int): StepProof = StepProof(
      proof = ctx.deduplicate(proof, premise, duplicate),
      premises = premises.take(duplicate) ++ premises.drop(duplicate + 1),
    )

    def weaken(premise: Int, sequent: Sequent): StepProof =
      StepProof(proof = ctx.weaken(proof, sequent), premises = premises :+ premise)

    /** Deduplicate all premises, favoring removing later premises if possible. */
    def deduplicateAll(): StepProof = {
      var result = this

      val seen = mutable.Map[Int, Int]()
      var i = 0
      while (i < result.premises.length) {
        val premise = result.premises(i)
        seen.get(premise) match {
          case Some(original) => result = result.deduplicate(original, i)
          case None =>
            seen.put(premise, i)
            i += 1
        }
      }

      result
    }

    /** Sort premises in descending order of id, i.e. in definition order (since later premises have smaller ids). */
    def sortDesc(): StepProof = {
      var result = this

      // A basic selection sort
      for (i <- proof.premises.indices.dropRight(1)) {
        val greatestAt = result.premises.zipWithIndex.drop(i).maxBy(_._1)._2
        if (i != greatestAt) result = result.swap(i, greatestAt)
      }

      result
    }
  }

  /** Known premises added so far. */
  private val premises = mutable.IndexedBuffer[Sequent]()

  /** Proofs for each step added so far. They rely only on input premises. */
  private val proofs = mutable.IndexedBuffer[StepProof]()

  private def conclusion(id: Int): Sequent = if (id < 0) premises(-id - 1) else proofs(id).proof.conclusion

  /** Add an input premise to the graph. */
  def premise(sequent: Sequent): Var = {
    val premiseVar = new Var(-premises.length - 1)
    premises.append(sequent)
    premiseVar
  }

  /**
   * Add a step to the graph. Refer to the step's conclusion by the returned [[Var]].
   *
   * @param proof
   *   The [[HippoProof]] representing the step consists of.
   * @param premises
   *   The variables that are connected to the premises of the step. These may be input premises of the graph or
   *   conclusions of previous steps.
   * @throws IllegalArgumentException
   *   - The number of proof premises and premise variables doesn't match.
   *   - A premise is inconsistent with its previous definition.
   */
  def step(proof: HippoProof, premises: Var*): Var = {
    require(proof.premises.length == premises.length)
    assert(premises.forall(_.id < proofs.size)) // Should already be true thanks to the static type system

    val stepVar = new Var(proofs.length)
    var stepProof = StepProof(proof, premises.map(_.id).toIndexedSeq)

    // First, ensure premises match existing conclusions.
    for ((premise, id) <- stepProof.proof.premises.zip(stepProof.premises)) require(premise.sequent == conclusion(id))

    // Then, insert all non-input proofs.
    // After this step, only input premises remain. By induction:
    // - The first added step must have only input premises
    //   because its id was 0 and a step's premise ids must be lower than its conclusion id.
    // - Every subsequent step's non-input premises were replaced with steps
    //   whose premises were all input premises.
    for ((id, i) <- stepProof.premises.zipWithIndex.reverseIterator if id >= 0)
      stepProof = stepProof.join(proofs(id), i)
    assert(stepProof.premises.forall(_ < 0))

    // Finally, deduplicate and sort premises.
    //
    // This approach doesn't produce a minimal sequence of operations.
    // For example, when encountering the sequence `2 1 2`,
    // it would first remove the second occurrence of `2` resulting in `2 1`,
    // and then swap the remaining two premises resulting in `1 2`.
    // An optimal approach would instead deduplicate the first occurrence of `2` resulting in `1 2` immediately.
    // I have no idea if this optimization is worth it for large graphs, or if it would make performance worse.
    stepProof = stepProof.deduplicateAll().sortDesc()

    proofs.append(stepProof)
    stepVar
  }

  /**
   * Construct a [[HippoProof]] from this graph.
   *
   * For more details on how the proof is constructed, see the class-level documentation for [[ProofGraph]].
   */
  def proof(of: Var): HippoProof = {
    var proof = if (of.id >= 0) proofs(of.id) else StepProof(ctx.sequent(premises(-of.id - 1)), IndexedSeq(of.id))

    // Ensure proof contains all known premises.
    val proofPremises = proof.premises.toSet
    for ((premise, i) <- premises.zipWithIndex) {
      val id = -i - 1
      if (!proofPremises.contains(id)) proof = proof.weaken(id, premise)
    }

    proof.sortDesc().proof
  }
}
