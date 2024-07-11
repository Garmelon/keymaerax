/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.axiom

import org.keymaerax.hippolochus.proof.HippoProof

trait HippoAxiom {
  def proof: HippoProof
  require(proof.subgoals.isEmpty)

  def infoOpt: Option[HippoAxiomInfo] = None
}
