/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.hippolang.namespace.ImmutableNamespace
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.{core, hippolochos}

sealed trait HippoValue

object HippoValue {
  final case object Null extends HippoValue

  final case class Bool(value: scala.Boolean) extends HippoValue

  final case class Int(value: scala.Int) extends HippoValue

  final case class String(value: java.lang.String) extends HippoValue

  final case class List(values: IndexedSeq[HippoValue]) extends HippoValue

  final case class DlExpression(value: core.Expression) extends HippoValue

  final case class DlSequent(value: core.Sequent) extends HippoValue

  final case class Namespace(value: ImmutableNamespace) extends HippoValue

  final case class Proof(value: HippoProof) extends HippoValue

  final case class Tactic(value: hippolochos.Tactic) extends HippoValue

  final case class ProofInfo(value: org.keymaerax.hippolib.meta.ProofInfo) extends HippoValue

  final case class TacticInfo(value: org.keymaerax.hippolib.meta.TacticInfo) extends HippoValue

  // Function-like

  final case class BuiltinFunction(value: org.keymaerax.hippolang.BuiltinFunction) extends HippoValue

  final case class BuiltinMemberFunction(target: HippoValue, value: org.keymaerax.hippolang.BuiltinMemberFunction)
      extends HippoValue

  final case class Function(env: ImmutableNamespace, args: Seq[HippoIdentifier], body: HippoExpression)
      extends HippoValue
}
