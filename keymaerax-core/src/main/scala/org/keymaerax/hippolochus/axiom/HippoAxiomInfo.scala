/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.axiom

import org.keymaerax.hippolochus.HippoIdentifier

case class HippoAxiomInfo(name: HippoIdentifier, description: Option[String])

object HippoAxiomInfo {
  def apply(name: String, description: String = ""): HippoAxiomInfo =
    HippoAxiomInfo(name = HippoIdentifier(name), description = Some(description.trim).filter(_.nonEmpty))
}
