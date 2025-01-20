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

/** Delegate to a forward or a backward tactic, depending on the execution direction. */
case class Bidi(forward: ForwardTactic, backward: BackwardTactic) extends ForwardTactic with BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(forward.hash).digest(backward.hash).hash

  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = ctx.forward(forward, premises)

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = ctx
    .backward(backward, conclusion, premises)
}
