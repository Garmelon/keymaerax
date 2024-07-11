/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.axiom

import org.keymaerax.hippolochus.proof.HippoProof

class RichAxiom[+T <: HippoAxiom](val info: HippoAxiomInfo, val inner: T) extends HippoAxiom {
  override def proof: HippoProof = inner.proof

  override def infoOpt: Option[HippoAxiomInfo] = Some(info)
}

object RichAxiom {
  def apply[T <: HippoAxiom](info: HippoAxiomInfo, inner: T): RichAxiom[T] = new RichAxiom(info, inner)
}
