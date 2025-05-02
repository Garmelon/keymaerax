/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.hippocore.PureTactic
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.Hash

case class CachedPure(tactic: PureTactic) extends PureTactic {
  override def hash: Hash = tactic.hash

  override def runPure(ctx: HippoContext): HippoProof = ctx.cachedPure(tactic)

  override def runForward(ctx: HippoContext, premises: IndexedSeq[HippoSequent]): HippoProof = ctx
    .cachedForward(tactic, premises)

  override def runBackward(ctx: HippoContext, conclusion: HippoSequent, premises: Map[Int, HippoSequent]): HippoProof =
    ctx.cachedBackward(tactic, conclusion, premises)
}
