/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.{HippoContext, ProofGraph}
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic, PureTactic, Tactic}

import scala.collection.mutable

class Graph private (private val steps: IndexedSeq[Graph.Step], private val conclusion: Int)
    extends PureTactic with ForwardTactic with BackwardTactic {
  override lazy val hash: Hash = Hasher()
    .digest[this.type]
    .digestSeqWith(steps) { (b, step) => b.digest(step.tactic.hash).digestSeqWith(step.premises)(_.digest(_)) }
    .hash

  def runAmbivalent(
      ctx: HippoContext,
      conclusion: Option[HippoSequent] = None,
      premises: Map[Int, HippoSequent] = Map(),
  ): HippoProof = {
    // Here, the sequents corresponding to the Builder's Vars are stored by their index.
    // They are used as input to the tactics during evaluation.
    val vars = mutable.Map[Int, HippoSequent]()
    for ((i, premise) <- premises) vars.put(-i - 1, premise)
    for (conclusion <- conclusion) vars.put(this.conclusion, conclusion)

    // Here, each step's proof is stored by the step's index.
    val stepProofs = mutable.Map[Int, HippoProof]()

    def addEqOrEmpty[T](map: mutable.Map[Int, T], key: Int, value: T): Unit = {
      val displaced = map.get(key)
      require(displaced.forall(_ == value))
      map.put(key, value)
    }

    def addProofForStep(i: Int, step: Graph.Step, proof: HippoProof): Unit = {
      addEqOrEmpty(stepProofs, i, proof)
      addEqOrEmpty(vars, i, proof.conclusion)
      require(step.premises.length == proof.premises.length)
      for ((i, premise) <- step.premises.zip(proof.premises)) addEqOrEmpty(vars, i, premise.sequent)
    }

    // First pass: Forward
    for ((step, i) <- steps.zipWithIndex) step.tactic match {
      // Evaluate all PureTactics
      case tactic: PureTactic => addProofForStep(i, step, ctx.pure(tactic))

      // Evaluate all ForwardTactics whose premises are all known
      case tactic: ForwardTactic if step.premises.forall(vars.contains) =>
        val premises = step.premises.map(vars)
        addProofForStep(i, step, ctx.forward(tactic, premises))

      case _ =>
    }

    // Second pass: Backward
    for ((step, i) <- steps.zipWithIndex.reverseIterator) step.tactic match {
      // Evaluate all BackwardTactics
      case tactic: BackwardTactic =>
        val conclusion = vars(i)
        val premises = step.premises.flatMap(i => vars.get(i).map(v => (i, v))).toMap
        addProofForStep(i, step, ctx.backward(tactic, conclusion, premises))

      // No other tactics should remain
      case _ => require(stepProofs.contains(i))
    }

    // Third pass: Build graph
    val graph = new ProofGraph(ctx)
    val graphVars = mutable.Map[Int, graph.Var]()
    for ((id, sequent) <- vars if id < 0) {
      val premiseVar = graph.premise(sequent)
      graphVars.put(id, premiseVar)
    }
    for ((step, id) <- steps.zipWithIndex) {
      val proof = stepProofs(id)
      val premises = step.premises.map(graphVars)
      val stepVar = graph.step(proof, premises: _*)
      graphVars.put(id, stepVar)
    }

    graph.proof(graphVars(this.conclusion))
  }

  override def runPure(ctx: HippoContext): HippoProof = runAmbivalent(ctx)

  override def runForward(ctx: HippoContext, premises: IndexedSeq[HippoSequent]): HippoProof =
    runAmbivalent(ctx, premises = premises.zipWithIndex.map(_.swap).toMap)

  override def runBackward(ctx: HippoContext, conclusion: HippoSequent, premises: Map[Int, HippoSequent]): HippoProof =
    runAmbivalent(ctx, conclusion = Some(conclusion), premises = premises)
}

object Graph {
  private case class Step(tactic: Tactic, premises: IndexedSeq[Int])

  def newBuilder: Builder = new Builder()

  class Builder private[Graph] {
    thisBuilder =>

    /** A [[Var]] represents a sequent in the graph. */
    final class Var private[Builder] (private[Builder] val i: Int) {

      /** Helper function for casting [[Var]]s. We should be able to remove it once we migrate to Scala 3. */
      def belongingTo(builder: Builder): builder.Var = {
        assert(builder eq thisBuilder)
        this.asInstanceOf[builder.Var]
      }
    }

    private val steps = mutable.Buffer[Step]()

    /** Refer to a premise based on its zero-based index. */
    def premise(i: Int): Var = {
      require(0 <= i)
      new Var(-i - 1)
    }

    /** Add a step to the graph. Refer to the step's conclusion by the returned [[Var]]. */
    def step(tactic: Tactic, premises: Var*): Var = {
      steps.append(Step(tactic = tactic, premises = premises.map(_.i).toIndexedSeq))
      new Var(steps.length - 1)
    }

    def stepAny(tactic: Tactic, premises: Seq[Builder#Var]): Var = {
      step(tactic, premises.map(_.belongingTo(this)): _*)
    }

    def build(conclusion: Var): Graph = {
      // This consistency check should already be true thanks to the static types.
      for {
        (step, i) <- steps.zipWithIndex
        premise <- step.premises
      } assert(premise < i, "step refers to later conclusion")

      new Graph(steps = steps.toIndexedSeq, conclusion = conclusion.i)
    }
  }
}
