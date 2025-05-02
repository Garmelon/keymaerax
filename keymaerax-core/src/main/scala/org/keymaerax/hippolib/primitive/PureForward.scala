/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippocore.{ForwardTactic, PureTactic}

/** Run a [[ForwardTactic]] like a [[PureTactic]] by manually supplying it with the required premises. */
case class PureForward(tactic: ForwardTactic, premises: IndexedSeq[HippoSequent]) extends PureTactic {
  override lazy val hash: Hash = Hasher()
    .digest[this.type]
    .digest(tactic.hash)
    .digestSeqWith(premises)(_.digest(_))
    .hash

  override def runPure(ctx: HippoContext): HippoProof = ctx.forward(tactic, premises)
}

object PureForward {
  def apply(tactic: ForwardTactic, premises: HippoSequent*): PureForward = PureForward(tactic, premises.toIndexedSeq)
}
