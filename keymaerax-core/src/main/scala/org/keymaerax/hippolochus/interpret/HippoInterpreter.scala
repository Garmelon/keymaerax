/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.interpret

import org.keymaerax.hippolochus.proof.{HippoGoal, HippoProof}
import org.keymaerax.hippolochus.{HippoExpression, HippoValue}

trait HippoInterpreter {
  def eval(expr: HippoExpression): HippoValue
  def prove(goal: HippoGoal, expr: HippoExpression): HippoProof
}
