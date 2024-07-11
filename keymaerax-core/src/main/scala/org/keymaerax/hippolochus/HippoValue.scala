/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus

import org.keymaerax.core
import org.keymaerax.hippolochus.axiom.HippoAxiom
import org.keymaerax.hippolochus.rule.HippoRule
import org.keymaerax.hippolochus.tactic.HippoTactic

sealed trait HippoValue

object HippoValue {
  final case object Null extends HippoValue

  final case class Bool(value: scala.Boolean) extends HippoValue

  final case class Int(value: scala.Int) extends HippoValue

  final case class String(value: java.lang.String) extends HippoValue

  final case class Dgl(value: core.Formula) extends HippoValue

  final case class Axiom(value: HippoAxiom) extends HippoValue

  final case class Rule(value: HippoRule) extends HippoValue

  final case class Tactic(value: HippoTactic) extends HippoValue

  final case class Builtin(value: HippoBuiltin) extends HippoValue

  final case class Namespace(values: Map[HippoIdentifier, HippoValue]) extends HippoValue

  // TODO Function (by func() ...)
}
