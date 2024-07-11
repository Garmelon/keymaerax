/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus

import org.keymaerax.hippolochus.axiom.{HippoAxiom, RichAxiom}
import org.keymaerax.hippolochus.library.{CoreAxioms, Tactics}
import org.keymaerax.hippolochus.rule.{HippoRule, RichRule}
import org.keymaerax.hippolochus.tactic.{HippoTactic, RichTactic}

case class HippoDb(
    axioms: Map[HippoIdentifier, HippoAxiom],
    rules: Map[HippoIdentifier, HippoRule],
    tactics: Map[HippoIdentifier, HippoTactic],
)

object HippoDb {
  private val allAxioms: Seq[RichAxiom[HippoAxiom]] = Seq(CoreAxioms.compose)
  private val allRules: Seq[RichRule[HippoRule]] = Seq()
  private val allTactics: Seq[RichTactic[HippoTactic]] = Seq(Tactics.sorry, Tactics.label, Tactics.axiom, Tactics.rule)

  val filledIn: HippoDb = HippoDb(
    axioms = allAxioms.map(a => a.info.name -> a).toMap,
    rules = allRules.map(r => r.info.name -> r).toMap,
    tactics = allTactics.map(t => t.info.name -> t).toMap,
  )
}
