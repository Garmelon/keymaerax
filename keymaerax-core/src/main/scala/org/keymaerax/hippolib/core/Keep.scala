/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.{AntePos, SeqPos, Sequent, SuccPos}
import org.keymaerax.hippocore.BackwardTactic
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippolib.HippoLib

case class Keep(parts: SeqPos*)(implicit lib: HippoLib) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digestSeqWith(parts)(_.digest(_)).hash

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = {
    val anteParts = parts.collect { case pos: AntePos => pos.getIndex }.distinct
    val succParts = parts.collect { case pos: SuccPos => pos.getIndex }.distinct
    val anteHide = conclusion.ante.indices.reverse.filter(!anteParts.contains(_)).map(AntePos(_))
    val succHide = conclusion.succ.indices.reverse.filter(!succParts.contains(_)).map(SuccPos(_))
    val hide: IndexedSeq[SeqPos] = anteHide ++ succHide
    ctx.backward(Hide(hide*), conclusion, premises)
  }
}
