/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic}
import org.keymaerax.hippocore.definitions.Name
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}

case class ExpandForward(names: Name*) extends ForwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digestSeq(names).hash

  override def runForward(ctx: HippoContext, premises: IndexedSeq[HippoSequent]): HippoProof = {
    val Seq(premise) = premises
    var chain = ctx.chain(premise)
    for (name <- names) chain = chain.joinForward(ctx.expandForward(chain.proof.conclusion, name))
    chain.proof
  }
}
