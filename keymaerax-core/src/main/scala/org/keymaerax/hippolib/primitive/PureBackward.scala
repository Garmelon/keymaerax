/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.core.Sequent
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.Hash
import org.keymaerax.hippolochos.{BackwardTactic, PureTactic}

/** Run a [[BackwardTactic]] like a [[PureTactic]] by manually supplying it with the required conclusion. */
case class PureBackward(tactic: BackwardTactic, conclusion: Sequent, premises: Map[Int, Sequent]) extends PureTactic {
  override lazy val hash: Hash = Hash
    .start
    .digest[this.type]
    .digest(tactic.hash)
    .digest(conclusion)
    .digestSeq(premises.toSeq.sortBy(_._1)) { case (b, (i, premise)) => b.digest(i).digest(premise) }
    .build

  override def runPure(ctx: HippoContext): HippoProof = ctx.backward(tactic, conclusion, premises)
}

object PureBackward {
  def apply(tactic: BackwardTactic, conclusion: Sequent, premises: (Int, Sequent)*): PureBackward =
    PureBackward(tactic, conclusion, premises.toMap)
}
