/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.interpret

import org.keymaerax.hippolochus.proof.{HippoGoal, HippoProof}
import org.keymaerax.hippolochus.{HippoExpression, HippoValue}

case class SequentialInterpreter(namespace: Namespace) extends HippoInterpreter {
  private val startState: SequentialInterpreterState =
    SequentialInterpreterState(interpreter = this, namespace = namespace)

  override def eval(expr: HippoExpression): HippoValue = {
    val (_, value) = startState.eval(expr)
    value
  }

  override def prove(goal: HippoGoal, expr: HippoExpression): HippoProof = {
    val (proof, _) = startState.prove(goal.asProof, expr)
    proof
  }
}
