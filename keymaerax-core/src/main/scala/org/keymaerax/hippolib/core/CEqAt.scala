/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core
import org.keymaerax.core.{AnyArg, Real, UnitFunctional, UnitPredicational}
import org.keymaerax.hippocore.BackwardTactic
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{ExprPath, Hash, Hasher}
import org.keymaerax.hippolib.HippoLib

case class CEqAt(at: ExprPath)(implicit lib: HippoLib) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(at).hash

  override def runBackward(
      ctx: HippoContext,
      conclusion: HippoSequent,
      premises: Map[Int, HippoSequent],
  ): HippoProof = {
    require(conclusion.sequent.succ.length == 1)
    require(conclusion.sequent.succ.head.isInstanceOf[core.Equiv])
    val concF = conclusion.sequent.succ.head.asInstanceOf[core.Equiv]
    val leftInner = at.select(concF.left)
    val rightInner = at.select(concF.right)

    leftInner match {
      case _: core.Term =>
        require(rightInner.isInstanceOf[core.Term])

        // See AxiomBase.scala
        val f_ = UnitFunctional("f_", AnyArg, Real)
        val g_ = UnitFunctional("g_", AnyArg, Real)
        val ctx_ = core.Function("ctx_", None, core.Real, core.Bool)

        ctx.uSubst(
          lib.core.CQrule.proof,
          f_ -> leftInner,
          g_ -> rightInner,
          core.PredOf(ctx_, core.DotTerm()) -> at.replace(concF.left, core.DotTerm()),
        )
      case _: core.Formula =>
        require(rightInner.isInstanceOf[core.Formula])

        // See AxiomBase.scala
        val p_ = UnitPredicational("p_", AnyArg)
        val q_ = UnitPredicational("q_", AnyArg)
        val ctx_ = core.Function("ctx_", None, core.Bool, core.Bool)

        ctx.uSubst(
          lib.core.CErule.proof,
          p_ -> leftInner,
          q_ -> rightInner,
          core.PredicationalOf(ctx_, core.DotFormula) -> at.replace(concF.left, core.DotFormula),
        )
      case _ => ???
    }
  }
}
