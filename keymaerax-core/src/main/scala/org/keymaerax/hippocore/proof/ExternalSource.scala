/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.proof

import org.keymaerax.core.{Formula, Provable}
import org.keymaerax.hippocore.definitions.Definitions
import org.keymaerax.hippocore.tools.{Hash, Hashable, Hasher}

sealed trait ExternalSource extends Hashable
object ExternalSource {
  case object Sorry extends ExternalSource {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type]
  }

  case class QeTool(formula: Formula, defs: Definitions) extends ExternalSource {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(formula).digest(defs)
  }

  case class Bellerophon(provable: Provable, defs: Definitions) extends ExternalSource {
    override def digestInto(hasher: Hasher): Unit = hasher
      .digest[this.type]
      .digest(provable.conclusion)
      .digestSeqWith(provable.subgoals)(_.digest(_))
      .digest(defs)
  }

  case class Cache(hash: Hash) extends ExternalSource {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(hash)
  }
}
