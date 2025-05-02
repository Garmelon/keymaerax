/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippocore.{BackwardTactic, PureTactic}

/** Run a [[BackwardTactic]] like a [[PureTactic]] by manually supplying it with the required conclusion. */
case class PureBackward(tactic: BackwardTactic, conclusion: HippoSequent, premises: Map[Int, HippoSequent])
    extends PureTactic {
  override lazy val hash: Hash = Hasher()
    .digest[this.type]
    .digest(tactic.hash)
    .digest(conclusion)
    .digestSeqWith(premises.toSeq.sortBy(_._1)) { case (b, (i, premise)) => b.digest(i).digest(premise) }
    .hash

  override def runPure(ctx: HippoContext): HippoProof = ctx.backward(tactic, conclusion, premises)
}

object PureBackward {
  def apply(tactic: BackwardTactic, conclusion: HippoSequent, premises: (Int, HippoSequent)*): PureBackward =
    PureBackward(tactic, conclusion, premises.toMap)
}
