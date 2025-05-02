/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.core.Sequent
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic}

/**
 * Run a backward tactic forwards by providing a fixed conclusion, using the premises as hints.
 *
 * Uses the original tactic without modification when run backwards.
 */
case class BidiBackward(tactic: BackwardTactic, conclusion: HippoSequent) extends ForwardTactic with BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(tactic.hash).digest(conclusion).hash

  override def runForward(ctx: HippoContext, premises: IndexedSeq[HippoSequent]): HippoProof = {
    val premiseMap = premises.zipWithIndex.map { case (p, i) => (i, p) }.toMap
    ctx.backward(tactic, conclusion, premiseMap)
  }

  override def runBackward(ctx: HippoContext, conclusion: HippoSequent, premises: Map[Int, HippoSequent]): HippoProof =
    ctx.backward(tactic, conclusion, premises)
}
