/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.tactic

import org.keymaerax.hippolochus.interpret.{CriticalException, HippoInterpreter}
import org.keymaerax.hippolochus.proof.{HippoGoal, HippoProof}
import org.keymaerax.hippolochus.{HippoExpression, HippoValue}

class Arg0Tactic(function: (HippoInterpreter, HippoGoal) => HippoProof) extends HippoTactic {
  override def executeWithArgs(interpreter: HippoInterpreter, goal: HippoGoal, args: Seq[HippoValue]): HippoProof = {
    CriticalException.args0(args)
    function(interpreter, goal)
  }

  def apply(interpreter: HippoInterpreter, goal: HippoGoal): HippoProof = function(interpreter, goal)
}

object Arg0Tactic {
  def apply(function: (HippoInterpreter, HippoGoal) => HippoProof): Arg0Tactic = new Arg0Tactic(function)

  def apply(function: HippoGoal => HippoProof): Arg0Tactic = Arg0Tactic { (_, goal) => function(goal) }

  def dependent(function: HippoGoal => HippoExpression): Arg0Tactic = Arg0Tactic { (i, goal) =>
    i.prove(goal, function(goal))
  }
}
