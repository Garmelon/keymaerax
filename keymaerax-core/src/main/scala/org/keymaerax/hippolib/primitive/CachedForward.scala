/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.core.Sequent
import org.keymaerax.hippolochos.ForwardTactic
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.Hash

case class CachedForward(tactic: ForwardTactic) extends ForwardTactic {
  override def hash: Hash = tactic.hash

  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = ctx
    .cachedForward(tactic, premises)
}
