/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.rule

import org.keymaerax.hippolochus.proof.HippoProof

class RichRule[+T <: HippoRule](val info: HippoRuleInfo, val inner: T) extends HippoRule {
  override def proof: HippoProof = inner.proof

  override def infoOpt: Option[HippoRuleInfo] = Some(info)
}

object RichRule {
  def apply[T <: HippoRule](info: HippoRuleInfo, inner: T): RichRule[T] = new RichRule(info, inner)
}
