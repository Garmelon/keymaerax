/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.hippocore.BackwardTactic
import org.keymaerax.hippocore.definitions.Name
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}

case class Expand(names: Name*) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digestSeq(names).hash

  override def runBackward(
      ctx: HippoContext,
      conclusion: HippoSequent,
      premises: Map[Int, HippoSequent],
  ): HippoProof = {
    var chain = ctx.chain(conclusion)
    for (name <- names) chain = chain.joinBackward(ctx.expand(chain.proof.premises.head.sequent, name))
    chain.proof
  }
}
