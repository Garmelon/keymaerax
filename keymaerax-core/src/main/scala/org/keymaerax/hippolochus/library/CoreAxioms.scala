/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.library

import org.keymaerax.core.Provable
import org.keymaerax.hippolochus.axiom.{HippoAxiomInfo, PureAxiom, RichAxiom}
import org.keymaerax.hippolochus.proof.HippoProof

object CoreAxioms {
  private def create(info: HippoAxiomInfo, coreName: String): RichAxiom[PureAxiom] =
    RichAxiom(info, PureAxiom(HippoProof.fromProvable(Provable.axioms(coreName))))

  val compose: RichAxiom[PureAxiom] = create(HippoAxiomInfo(name = "compose"), "[;] compose")
}
