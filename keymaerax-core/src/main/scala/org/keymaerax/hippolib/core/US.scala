/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.hippolib.publish
import org.keymaerax.core.{Expression, Sequent, SubstitutionPair, USubst}
import org.keymaerax.hippolib.meta.{TacticArg, TacticArgInfo, TacticInfo}
import org.keymaerax.hippolochos.ForwardTactic
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext

case class US(subst: USubst) extends ForwardTactic {
  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = {
    require(premises.length == 1)
    val Seq(premise) = premises
    ctx.uSubstGlobal(premise, subst)
  }
}

object US {
  def apply(substs: (Expression, Expression)*): US = US(USubst(substs.map { case (from, to) =>
    SubstitutionPair(from, to)
  }))

  @publish(name = "core.US")
  val info: TacticInfo = TacticInfo(
    TacticArgInfo(name = "subst", arg = TacticArg.Seq(TacticArg.Tuple2(TacticArg.Expression, TacticArg.Expression)))
  ) { substs => US(substs: _*) }
}
