/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.parse

import org.keymaerax.hippolang.HlangIdentifier

case class AstIdentifier(name: HlangIdentifier) {
  override def toString: String = name.quoted
}

object AstIdentifier {
  def apply(name: String): AstIdentifier = AstIdentifier(HlangIdentifier(name))
}
