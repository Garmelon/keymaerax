/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.tactic
import org.keymaerax.hippolochus.HippoValue
import org.keymaerax.hippolochus.interpret.HippoInterpreter
import org.keymaerax.hippolochus.proof.{HippoGoal, HippoProof}

class RichTactic[+T <: HippoTactic](val info: HippoTacticInfo, val inner: T) extends HippoTactic {
  override def executeWithArgs(interpreter: HippoInterpreter, goal: HippoGoal, args: Seq[HippoValue]): HippoProof =
    inner.executeWithArgs(interpreter, goal, args)

  override def infoOpt: Option[HippoTacticInfo] = Some(info)
}

object RichTactic {
  def apply[T <: HippoTactic](info: HippoTacticInfo, inner: T): RichTactic[T] =
    new RichTactic(info = info, inner = inner)
}
