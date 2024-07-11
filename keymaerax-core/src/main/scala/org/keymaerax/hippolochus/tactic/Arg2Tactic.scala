/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.tactic

import org.keymaerax.hippolochus.interpret.{CriticalException, HippoInterpreter}
import org.keymaerax.hippolochus.proof.{HippoGoal, HippoProof}
import org.keymaerax.hippolochus.{HippoExpression, HippoValue}

import scala.reflect.ClassTag

class Arg2Tactic[T1 <: HippoValue: ClassTag, T2 <: HippoValue: ClassTag](
    function: (HippoInterpreter, HippoGoal, T1, T2) => HippoProof
) extends HippoTactic {

  override def executeWithArgs(interpreter: HippoInterpreter, goal: HippoGoal, args: Seq[HippoValue]): HippoProof = {
    val (arg1, arg2) = CriticalException.args2[T1, T2](args)
    function(interpreter, goal, arg1, arg2)
  }

  def apply(interpreter: HippoInterpreter, goal: HippoGoal, arg1: T1, arg2: T2): HippoProof =
    function(interpreter, goal, arg1, arg2)
}

object Arg2Tactic {
  def apply[T1 <: HippoValue: ClassTag, T2 <: HippoValue: ClassTag](
      function: (HippoInterpreter, HippoGoal, T1, T2) => HippoProof
  ): Arg2Tactic[T1, T2] = new Arg2Tactic(function)

  def apply[T1 <: HippoValue: ClassTag, T2 <: HippoValue: ClassTag](
      function: (HippoGoal, T1, T2) => HippoProof
  ): Arg2Tactic[T1, T2] = Arg2Tactic { (_, goal, arg1, arg2) => function(goal, arg1, arg2) }

  def dependent[T1 <: HippoValue: ClassTag, T2 <: HippoValue: ClassTag](
      function: (HippoGoal, T1, T2) => HippoExpression
  ): Arg2Tactic[T1, T2] = Arg2Tactic { (i, goal, arg1, arg2) => i.prove(goal, function(goal, arg1, arg2)) }
}
