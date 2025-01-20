/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.tools.{Hashable, Hasher}

case class ProofInfo(proof: HippoProof) extends Hashable {
  override def digestInto(hasher: Hasher): Unit = hasher.digest(proof)
}
