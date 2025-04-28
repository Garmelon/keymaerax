/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.{AntePos, HideLeft, HideRight, SeqPos, Sequent, SuccPos}
import org.keymaerax.hippocore.BackwardTactic
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippolib.HippoLib

case class Hide(parts: SeqPos*)(implicit lib: HippoLib) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digestSeqWith(parts)(_.digest(_)).hash

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = {
    val anteParts = parts.collect { case pos: AntePos => pos }.distinct.sortBy(_.getIndex).reverse
    val succParts = parts.collect { case pos: SuccPos => pos }.distinct.sortBy(_.getIndex).reverse
    require(anteParts.forall(_.getIndex < conclusion.ante.length))
    require(succParts.forall(_.getIndex < conclusion.succ.length))

    var chain = ctx.chain(conclusion)
    for (pos <- anteParts) chain = chain.backward(CoreRule(HideLeft(pos)))
    for (pos <- succParts) chain = chain.backward(CoreRule(HideRight(pos)))
    chain.proof
  }
}
