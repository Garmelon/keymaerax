/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic}

/** Delegate to a forward or a backward tactic, depending on the execution direction. */
case class Bidi(forward: ForwardTactic, backward: BackwardTactic) extends ForwardTactic with BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(forward.hash).digest(backward.hash).hash

  override def runForward(ctx: HippoContext, premises: IndexedSeq[HippoSequent]): HippoProof = ctx
    .forward(forward, premises)

  override def runBackward(ctx: HippoContext, conclusion: HippoSequent, premises: Map[Int, HippoSequent]): HippoProof =
    ctx.backward(backward, conclusion, premises)
}
