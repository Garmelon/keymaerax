/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.{Rule, Sequent}
import org.keymaerax.hippolochos.BackwardTactic
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext

/** A tactic that can apply [[Rule]]s from the core. */
case class CoreRule(rule: Rule) extends BackwardTactic {
  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = ctx
    .coreProofRule(rule, conclusion)
}
