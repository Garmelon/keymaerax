/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.{Rule, Sequent}
import org.keymaerax.hippolochos.BackwardTactic
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.{Hash, Hasher}

/** A tactic that can apply [[Rule]]s from the core. */
case class CoreRule(rule: Rule) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(rule).hash

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = ctx
    .coreProofRule(rule, conclusion)
}
