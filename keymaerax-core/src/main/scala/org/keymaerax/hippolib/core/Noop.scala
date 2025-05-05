/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic}

/**
 * Do nothing, if possible.
 *
 * When run forwards, must be called with exactly one premise which it then returns as conclusion. When run backwards,
 * returns its conclusion as its only premise.
 */
case object Noop extends ForwardTactic with BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].hash

  override def runForward(ctx: HippoContext, premises: IndexedSeq[HippoSequent]): HippoProof = {
    require(premises.length == 1)
    HippoProof.Sequent(premises.head)
  }

  override def runBackward(ctx: HippoContext, conclusion: HippoSequent, premises: Map[Int, HippoSequent]): HippoProof =
    ctx.sequent(conclusion)
}
