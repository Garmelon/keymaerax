/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.hippocore.BackwardTactic
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.infrastruct.UnificationMatch

case class Unify(proof: HippoProof) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(proof).hash

  require(proof.conclusion.sequent.ante.isEmpty)
  require(proof.conclusion.sequent.succ.length == 1)

  private val Seq(proofF) = proof.conclusion.sequent.succ

  override def runBackward(
      ctx: HippoContext,
      conclusion: HippoSequent,
      premises: Map[Int, HippoSequent],
  ): HippoProof = {
    require(conclusion.sequent.ante.isEmpty)
    require(conclusion.sequent.succ.length == 1)

    val Seq(concF) = conclusion.sequent.succ

    UnificationMatch(proofF, concF).toHippo(proof)
  }
}
