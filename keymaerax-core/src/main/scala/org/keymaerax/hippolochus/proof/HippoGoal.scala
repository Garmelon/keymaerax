/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.proof

import org.keymaerax.core.Sequent

case class HippoGoal(sequent: Sequent, label: Option[String] = None) {
  def asProof: HippoProof = HippoProof.fromGoal(this)
}
