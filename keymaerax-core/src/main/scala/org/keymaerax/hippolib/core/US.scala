/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.{Expression, Sequent, SubstitutionPair, USubst}
import org.keymaerax.hippolochos.ForwardTactic
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.{Hash, Hasher}

case class US(subst: USubst) extends ForwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(subst).hash

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
}
