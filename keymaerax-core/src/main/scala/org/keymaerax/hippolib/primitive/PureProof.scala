/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.hippolochos.PureTactic
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext

case class PureProof(proof: HippoProof) extends PureTactic {
  override def runPure(ctx: HippoContext): HippoProof = proof
}
