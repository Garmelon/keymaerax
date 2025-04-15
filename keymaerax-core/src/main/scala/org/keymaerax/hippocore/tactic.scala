/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore

import org.keymaerax.core.Sequent
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hashable, Hasher}

sealed trait Tactic extends Hashable {
  def hash: Hash

  final override def digestInto(hasher: Hasher): Unit = hasher.digest(hash)
}

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
