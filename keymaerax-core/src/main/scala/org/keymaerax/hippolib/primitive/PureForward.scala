/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.core.Sequent
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.{ForwardTactic, PureTactic}

/** Run a [[ForwardTactic]] like a [[PureTactic]] by manually supplying it with the required premises. */
case class PureForward(tactic: ForwardTactic, premises: IndexedSeq[Sequent]) extends PureTactic {
  override def runPure(ctx: HippoContext): HippoProof = ctx.forward(tactic, premises)
}

object PureForward {
  def apply(tactic: ForwardTactic, premises: Sequent*): PureForward = PureForward(tactic, premises.toIndexedSeq)
}
