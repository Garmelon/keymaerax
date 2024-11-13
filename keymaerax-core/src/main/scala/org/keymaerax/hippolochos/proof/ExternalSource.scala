/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.proof

import org.keymaerax.core.Formula
import org.keymaerax.hippolochos.tools.Hash

sealed trait ExternalSource
object ExternalSource {
  case object Sorry extends ExternalSource
  case class QeTool(formula: Formula) extends ExternalSource
  case class Cache(hash: Hash) extends ExternalSource
}
