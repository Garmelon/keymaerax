/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.proof

import org.keymaerax.core.{Formula, Sequent, URename, USubst}
import org.keymaerax.hippocore.definitions.Definitions
import org.keymaerax.hippocore.tools.{Hashable, Hasher}

case class HippoSequent private (sequent: Sequent, defs: Definitions) extends Hashable {
  def sequentExpanded: Sequent = defs.expandAll(sequent)

  def applyRename(rename: URename): HippoSequent = HippoSequent(rename(sequent), defs.applyRename(rename))
  def applySubst(subst: USubst): HippoSequent = HippoSequent(subst(sequent), defs.applySubst(subst))
  def applySubstAllTaboo(subst: USubst): HippoSequent = HippoSequent(subst(sequent), defs.applySubstAllTaboo(subst))

  override def digestInto(hasher: Hasher): Unit = hasher.digest(sequent).digest(defs)
}

object HippoSequent {
  def apply(sequent: Sequent, defs: Definitions = Definitions.empty): HippoSequent =
    new HippoSequent(sequent = sequent, defs = defs.retainReachable(sequent))
}
