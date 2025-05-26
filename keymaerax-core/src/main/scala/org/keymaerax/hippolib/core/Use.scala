/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.{CoHideRight, Cut, SuccPos}
import org.keymaerax.hippocore.BackwardTactic
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippolib.HippoLib
import org.keymaerax.hippolib.belle.Belle

/**
 * Use an existing proof to prove (or at least transform) the current conclusion sequent. Uses
 * [[org.keymaerax.btactics.TactixLibrary.prop]] internally to prove that `proof` proves the current conclusion.
 * {{{
 *    proof
 *   -------- Use(proof)
 *    G |- D
 * }}}
 *
 * How it works:
 * {{{
 *                                 proof
 *                                ------- Unpack
 *        *                        |- p
 *   ----------- Belle(prop)    ----------- CoHideRight
 *    G, p |- D                  G |- D, p
 *   -------------------------------------- Cut
 *                   G |- D
 * }}}
 */
case class Use(proof: HippoProof)(implicit lib: HippoLib) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(proof).hash

  override def runBackward(
      ctx: HippoContext,
      conclusion: HippoSequent,
      premises: Map[Int, HippoSequent],
  ): HippoProof = {
    val fmlProof = ctx.forwardJoin(Unpack, proof)

    assert(fmlProof.conclusion.sequent.ante.isEmpty)
    assert(fmlProof.conclusion.sequent.succ.length == 1)
    val fml = fmlProof.conclusion.sequent.succ(0)
    val result = ctx
      .chain(conclusion)
      .backward(CoreRule(Cut(fml)))
      .backward(Belle("prop") /* Should eliminate first premise */ )
      .backward(CoreRule(CoHideRight(SuccPos(conclusion.sequent.succ.length))))
      .joinBackward(fmlProof /* Might eliminate second premise */ )
      .proof

    assert(result.premises.length <= 1)
    result
  }
}
