/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.Sequent
import org.keymaerax.hippolochos.BackwardTactic
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.Hash
import org.keymaerax.infrastruct.UnificationMatch

case class Unify(proof: HippoProof) extends BackwardTactic {
  override lazy val hash: Hash = Hash.start.digest[this.type].digest(proof).build

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
