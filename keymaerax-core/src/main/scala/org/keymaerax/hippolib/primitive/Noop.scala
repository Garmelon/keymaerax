/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.core.Sequent
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.{BackwardTactic, ForwardTactic}

/**
 * Do nothing, if possible.
 *
 * When run forwards, must be called with exactly one premise which it then returns as conclusion. When run backwards,
 * returns its conclusion as its only premise.
 */
case object Noop extends ForwardTactic with BackwardTactic {
  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = {
    require(premises.length == 1)
    HippoProof.Sequent(premises.head)
  }

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = ctx
    .sequent(conclusion)
}
