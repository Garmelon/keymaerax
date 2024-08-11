/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos

import org.keymaerax.core.Sequent
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext

sealed trait Tactic

trait ForwardTactic extends Tactic {
  def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof
}

trait BackwardTactic extends Tactic {
  def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof
}

trait PureTactic extends Tactic with ForwardTactic with BackwardTactic {
  def runPure(ctx: HippoContext): HippoProof

  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = runPure(ctx)
  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof =
    runPure(ctx)
}
