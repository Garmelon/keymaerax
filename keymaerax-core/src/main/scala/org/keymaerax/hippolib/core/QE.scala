/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.{CloseTrue, SuccPos}
import org.keymaerax.hippocore.BackwardTactic
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{ExprPath, Hash, Hasher}
import org.keymaerax.hippolib.HippoLib

case class QE()(implicit lib: HippoLib) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].hash

  override def runBackward(
      ctx: HippoContext,
      conclusion: HippoSequent,
      premises: Map[Int, HippoSequent],
  ): HippoProof = {
    require(conclusion.sequent.ante.isEmpty)
    require(conclusion.sequent.succ.length == 1)
    val Seq(formula) = conclusion.sequent.succ

    val qeProof = ctx.qe(formula)

    ctx
      .chain(conclusion)
      .backwardJoin(RewriteAt(ExprPath()), 1 -> qeProof)
      .backward(CoreRule(CloseTrue(SuccPos(0))))
      .proof
  }
}
