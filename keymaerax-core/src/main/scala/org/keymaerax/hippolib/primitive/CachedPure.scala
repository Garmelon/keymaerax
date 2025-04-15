/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.core.Sequent
import org.keymaerax.hippocore.PureTactic
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.Hash

case class CachedPure(tactic: PureTactic) extends PureTactic {
  override def hash: Hash = tactic.hash

  override def runPure(ctx: HippoContext): HippoProof = ctx.cachedPure(tactic)

  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = ctx
    .cachedForward(tactic, premises)

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = ctx
    .cachedBackward(tactic, conclusion, premises)
}
