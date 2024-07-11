/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus

sealed trait HippoExpression

object HippoExpression {
  // Primitive
  case class Const(value: HippoValue) extends HippoExpression
  case class Theorem(statement: HippoExpression, proof: HippoExpression) extends HippoExpression
  case class On(label: HippoValue, proof: HippoExpression) extends HippoExpression
  case class Fork(cases: Seq[On]) extends HippoExpression
  case object Done extends HippoExpression
  case class Lookup(name: HippoIdentifier) extends HippoExpression
  case class Assign(name: HippoIdentifier, value: HippoExpression) extends HippoExpression
  case class Sequence(first: HippoExpression, second: HippoExpression) extends HippoExpression
  case class Block(inner: HippoExpression) extends HippoExpression

  // Suffix
  case class Access(target: HippoExpression, name: HippoIdentifier) extends HippoExpression
  case class Apply(target: HippoExpression, args: IndexedSeq[HippoExpression]) extends HippoExpression
}
