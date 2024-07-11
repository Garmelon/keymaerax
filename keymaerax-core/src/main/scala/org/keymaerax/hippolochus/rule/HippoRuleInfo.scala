/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.rule

import org.keymaerax.hippolochus.HippoIdentifier

case class HippoRuleInfo(name: HippoIdentifier, description: Option[String])

object HippoRuleInfo {
  def apply(name: String, description: String = ""): HippoRuleInfo =
    HippoRuleInfo(name = HippoIdentifier(name), description = Some(description.trim).filter(_.nonEmpty))
}
