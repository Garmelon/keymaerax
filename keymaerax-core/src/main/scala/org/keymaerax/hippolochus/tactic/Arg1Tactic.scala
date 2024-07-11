/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.tactic

import org.keymaerax.hippolochus.interpret.{CriticalException, HippoInterpreter}
import org.keymaerax.hippolochus.proof.{HippoGoal, HippoProof}
import org.keymaerax.hippolochus.{HippoExpression, HippoValue}

import scala.reflect.ClassTag

class Arg1Tactic[T1 <: HippoValue: ClassTag](function: (HippoInterpreter, HippoGoal, T1) => HippoProof)
    extends HippoTactic {

  override def executeWithArgs(interpreter: HippoInterpreter, goal: HippoGoal, args: Seq[HippoValue]): HippoProof = {
    val arg1 = CriticalException.args1[T1](args)
    function(interpreter, goal, arg1)
  }

  def apply(interpreter: HippoInterpreter, goal: HippoGoal, arg1: T1): HippoProof = function(interpreter, goal, arg1)
}

object Arg1Tactic {
  def apply[T1 <: HippoValue: ClassTag](function: (HippoInterpreter, HippoGoal, T1) => HippoProof): Arg1Tactic[T1] =
    new Arg1Tactic(function)

  def apply[T1 <: HippoValue: ClassTag](function: (HippoGoal, T1) => HippoProof): Arg1Tactic[T1] =
    Arg1Tactic { (_, goal, arg1) => function(goal, arg1) }

  def dependent[T1 <: HippoValue: ClassTag](function: (HippoGoal, T1) => HippoExpression): Arg1Tactic[T1] =
    Arg1Tactic { (i, goal, arg1) => i.prove(goal, function(goal, arg1)) }
}
