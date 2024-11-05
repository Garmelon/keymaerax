/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.core.Sequent
import org.keymaerax.hippolochos.PureTactic
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.Hash

case class CachedPure(tactic: PureTactic) extends PureTactic {
  override def hash: Hash = tactic.hash

  override def runPure(ctx: HippoContext): HippoProof = ctx.cachedPure(tactic)

  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = ctx
    .cachedForward(tactic, premises)

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = ctx
    .cachedBackward(tactic, conclusion, premises)
}
