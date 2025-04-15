/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.tools.{Hashable, Hasher}

case class ProofInfo(proof: HippoProof) extends Hashable {
  override def digestInto(hasher: Hasher): Unit = hasher.digest(proof)
}
