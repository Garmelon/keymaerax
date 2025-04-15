/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.core.Sequent
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.Hash
import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic}

case class CachedBidi(tactic: ForwardTactic with BackwardTactic) extends ForwardTactic with BackwardTactic {
  override def hash: Hash = tactic.hash

  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = ctx
    .cachedForward(tactic, premises)

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = ctx
    .cachedBackward(tactic, conclusion, premises)
}
