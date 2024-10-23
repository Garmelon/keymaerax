/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.hippolochos.PureTactic
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.Hash

case class PureProof(proof: HippoProof) extends PureTactic {
  override lazy val hash: Hash = Hash.start.digest[this.type].digest(proof).build

  override def runPure(ctx: HippoContext): HippoProof = proof
}
