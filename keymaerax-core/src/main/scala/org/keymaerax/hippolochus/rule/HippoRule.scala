/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.rule

import org.keymaerax.hippolochus.proof.HippoProof

trait HippoRule {
  def proof: HippoProof
  require(proof.subgoals.nonEmpty)

  def infoOpt: Option[HippoRuleInfo] = None
}
