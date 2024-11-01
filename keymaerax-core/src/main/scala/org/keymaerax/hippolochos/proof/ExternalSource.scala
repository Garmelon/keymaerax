/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.proof

import org.keymaerax.core.Formula

sealed trait ExternalSource
object ExternalSource {
  case object Sorry extends ExternalSource
  case class QeTool(formula: Formula) extends ExternalSource
}
