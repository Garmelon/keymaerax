/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.proof

import org.keymaerax.core.Formula
import org.keymaerax.hippolochos.tools.{Hash, Hashable, Hasher}

sealed trait ExternalSource extends Hashable
object ExternalSource {
  case object Sorry extends ExternalSource {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type]
  }

  case class QeTool(formula: Formula) extends ExternalSource {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(formula)
  }

  case class Cache(hash: Hash) extends ExternalSource {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(hash)
  }
}
