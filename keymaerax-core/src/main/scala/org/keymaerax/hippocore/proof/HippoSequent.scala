/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.proof

import org.keymaerax.core.{Sequent, URename, USubst}
import org.keymaerax.hippocore.definitions.{Definitions, Name}
import org.keymaerax.hippocore.tools.{Hashable, Hasher}

case class HippoSequent private (sequent: Sequent, defs: Definitions) extends Hashable {
  def sequentExpanded: Sequent = defs.expandAll(sequent)

  def antes: IndexedSeq[HippoExpression] = sequent.ante.map(HippoExpression(_, defs))
  def succs: IndexedSeq[HippoExpression] = sequent.succ.map(HippoExpression(_, defs))
  def anteAt(i: Int): HippoExpression = HippoExpression(sequent.ante(i), defs)
  def succAt(i: Int): HippoExpression = HippoExpression(sequent.succ(i), defs)

  def expand(name: Name): HippoSequent = HippoSequent(defs.expand(name, sequent), defs)
  def expandAll: HippoSequent = HippoSequent(defs.expandAll(sequent), defs)

  def applyRename(rename: URename): HippoSequent = HippoSequent(rename(sequent), defs.applyRename(rename))
  def applySubst(subst: USubst): HippoSequent = HippoSequent(subst(sequent), defs.applySubst(subst))
  def applySubstAllTaboo(subst: USubst): HippoSequent = HippoSequent(subst(sequent), defs.applySubstAllTaboo(subst))

  override def digestInto(hasher: Hasher): Unit = hasher.digest(sequent).digest(defs)
}

object HippoSequent {
  def apply(sequent: Sequent, defs: Definitions = Definitions.empty): HippoSequent =
    new HippoSequent(sequent = sequent, defs = defs.retainReachable(sequent))
}
