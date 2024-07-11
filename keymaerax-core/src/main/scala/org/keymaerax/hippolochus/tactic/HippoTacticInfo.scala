/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.tactic

import org.keymaerax.hippolochus.HippoIdentifier

case class HippoTacticInfo(name: HippoIdentifier, description: Option[String])

object HippoTacticInfo {
  def apply(name: String, description: String = ""): HippoTacticInfo =
    HippoTacticInfo(name = HippoIdentifier(name), description = Some(description.trim).filter(_.nonEmpty))
}
