/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.library

import org.keymaerax.core.hippolochus.annotations.Tactic
import org.keymaerax.hippolochus.HippoValue
import org.keymaerax.hippolochus.proof.HippoProof
import org.keymaerax.hippolochus.tactic.{Arg0Tactic, Arg1Tactic, HippoTacticInfo, RichTactic}

object Tactics {
  @Tactic(name = "sorry")
  val sorry: RichTactic[Arg0Tactic] =
    RichTactic(HippoTacticInfo(name = "sorry"), Arg0Tactic { goal => goal.asProof.sorry() })

  @Tactic(name = "label")
  val label: RichTactic[Arg1Tactic[HippoValue.String]] = RichTactic(
    HippoTacticInfo(name = "label", description = "Change the current goal's label."),
    Arg1Tactic { (goal, name) => goal.asProof.label(0, Some(name.value)) },
  )

  @Tactic(name = "axiom")
  val axiom: RichTactic[Arg1Tactic[HippoValue.Axiom]] = RichTactic(
    HippoTacticInfo(name = "axiom", description = "Apply an axiom to the current proof."),
    Arg1Tactic { (goal, axiom) => goal.asProof.extend(0, HippoProof.fromAxiom(axiom.value)) },
  )

  @Tactic(name = "rule")
  val rule: RichTactic[Arg1Tactic[HippoValue.Rule]] = RichTactic(
    HippoTacticInfo(name = "rule", description = "Apply a rule to the current proof."),
    Arg1Tactic { (goal, rule) => goal.asProof.extend(0, HippoProof.fromRule(rule.value)) },
  )
}
