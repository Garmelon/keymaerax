/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.{Rule, Sequent}
import org.keymaerax.hippocore.BackwardTactic
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}

/** A tactic that can apply [[Rule]]s from the core. */
case class CoreRule(rule: Rule) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(rule).hash

  override def runBackward(ctx: HippoContext, conclusion: HippoSequent, premises: Map[Int, HippoSequent]): HippoProof =
    ctx.coreProofRule(rule, conclusion)
}
