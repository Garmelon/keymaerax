/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.core.Sequent
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.{Hash, Hasher}
import org.keymaerax.hippolochos.{BackwardTactic, ForwardTactic}

/**
 * Run a forward tactic backwards by providing fixed premises.
 *
 * Uses the original tactic without modification when run forwards.
 */
case class BidiForward(tactic: ForwardTactic, premises: Map[Int, Sequent]) extends ForwardTactic with BackwardTactic {
  override lazy val hash: Hash = Hasher()
    .digest[this.type]
    .digest(tactic.hash)
    .digestSeqWith(premises.toSeq.sortBy(_._1)) { case (b, (i, premise)) => b.digest(i).digest(premise) }
    .hash

  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = ctx.forward(tactic, premises)

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = {
    val premiseList = (this.premises ++ premises).toIndexedSeq.sortBy(_._1).map(_._2)
    ctx.forward(tactic, premiseList)
  }
}

object BidiForward {
  def apply(tactic: ForwardTactic, premises: (Int, Sequent)*): BidiForward = BidiForward(tactic, premises.toMap)
}
