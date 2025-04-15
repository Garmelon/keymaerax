/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.core.Sequent
import org.keymaerax.hippocore.BackwardTactic
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.Hash

case class CachedBackward(tactic: BackwardTactic) extends BackwardTactic {
  override def hash: Hash = tactic.hash

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = ctx
    .cachedBackward(tactic, conclusion, premises)
}
