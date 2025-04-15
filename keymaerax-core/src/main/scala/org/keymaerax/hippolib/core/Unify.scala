/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.Sequent
import org.keymaerax.hippocore.BackwardTactic
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.infrastruct.UnificationMatch

case class Unify(proof: HippoProof) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(proof).hash

  require(proof.conclusion.ante.isEmpty)
  require(proof.conclusion.succ.length == 1)

  private val Seq(proofF) = proof.conclusion.succ

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = {
    require(conclusion.ante.isEmpty)
    require(conclusion.succ.length == 1)

    val Seq(concF) = conclusion.succ

    UnificationMatch(proofF, concF).toHippo(proof)
  }
}
