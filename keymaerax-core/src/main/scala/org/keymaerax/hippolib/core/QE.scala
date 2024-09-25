/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.hippolib.publish
import org.keymaerax.core.{CloseTrue, Sequent, SuccPos}
import org.keymaerax.hippolib.meta.TacticInfo
import org.keymaerax.hippolochos.BackwardTactic
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.ExprPath

object QE extends BackwardTactic {
  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = {
    require(conclusion.ante.isEmpty)
    require(conclusion.succ.length == 1)
    val Seq(formula) = conclusion.succ

    val qeProof = ctx.qe(formula)

    ctx
      .chain(conclusion)
      .backwardJoin(RewriteAt(ExprPath()), 1 -> qeProof)
      .backward(CoreRule(CloseTrue(SuccPos(0))))
      .proof
  }

  @publish(name = "core.QE")
  val info: TacticInfo = TacticInfo(QE)
}
