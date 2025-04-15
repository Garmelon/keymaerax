/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.core.Sequent
import org.keymaerax.hippocore.ForwardTactic
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.Hash

case class CachedForward(tactic: ForwardTactic) extends ForwardTactic {
  override def hash: Hash = tactic.hash

  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = ctx
    .cachedForward(tactic, premises)
}
