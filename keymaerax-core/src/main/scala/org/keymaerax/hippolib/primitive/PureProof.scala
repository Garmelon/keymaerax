/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.hippocore.PureTactic
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}

case class PureProof(proof: HippoProof) extends PureTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(proof).hash

  override def runPure(ctx: HippoContext): HippoProof = proof
}
