/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.tactic

import org.keymaerax.hippolochus.HippoValue
import org.keymaerax.hippolochus.interpret.HippoInterpreter
import org.keymaerax.hippolochus.proof.{HippoGoal, HippoProof}

trait HippoTactic {
  def executeWithArgs(interpreter: HippoInterpreter, goal: HippoGoal, args: Seq[HippoValue]): HippoProof

  def infoOpt: Option[HippoTacticInfo] = None
}
