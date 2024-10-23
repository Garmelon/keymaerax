/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.core.Sequent
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.Hash
import org.keymaerax.hippolochos.{BackwardTactic, ForwardTactic}

/**
 * Run a backward tactic forwards by providing a fixed conclusion, using the premises as hints.
 *
 * Uses the original tactic without modification when run backwards.
 */
case class BidiBackward(tactic: BackwardTactic, conclusion: Sequent) extends ForwardTactic with BackwardTactic {
  override lazy val hash: Hash = Hash.start.digest[this.type].digest(tactic.hash).digest(conclusion).build

  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = {
    val premiseMap = premises.zipWithIndex.map { case (p, i) => (i, p) }.toMap
    ctx.backward(tactic, conclusion, premiseMap)
  }

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = ctx
    .backward(tactic, conclusion, premises)
}
