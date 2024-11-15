/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.hippolang.parse.SourceFile

sealed trait HippoExpression

object HippoExpression {
  ///////////////
  // Primitive //
  ///////////////

  case class Const(value: HippoValue) extends HippoExpression

  case class Import(path: HippoExpression, slice: SourceFile#Slice) extends HippoExpression

  case class Declare(
      exportSlice: Option[SourceFile#Slice],
      mutable: Boolean,
      name: HippoIdentifier,
      value: HippoExpression,
  ) extends HippoExpression {
    def exports: Boolean = exportSlice.isDefined
  }

  case class Assign(name: HippoIdentifier, value: HippoExpression) extends HippoExpression

  case class Lookup(name: HippoIdentifier) extends HippoExpression

  case class If(condition: HippoExpression, ifTrue: HippoExpression, ifFalse: Option[HippoExpression])
      extends HippoExpression

  case class While(condition: HippoExpression, body: HippoExpression) extends HippoExpression

  case class Function(args: Seq[HippoIdentifier], body: HippoExpression) extends HippoExpression

  case class Theorem(
      verifySlice: Option[SourceFile#Slice],
      conclusion: HippoExpression,
      premises: Seq[HippoExpression],
      proof: HippoExpression,
      proofSlice: SourceFile#Slice,
  ) extends HippoExpression

  case class Sequence(exprs: Seq[HippoExpression], returnExpr: Option[HippoExpression]) extends HippoExpression

  case class Block(inner: HippoExpression) extends HippoExpression

  case class BackwardBlock(inner: HippoExpression) extends HippoExpression

  case class GraphBlock(inner: HippoExpression) extends HippoExpression

  ////////////
  // Suffix //
  ////////////

  case class BuiltinAccess(target: HippoExpression, member: org.keymaerax.hippolang.BuiltinMemberFunction)
      extends HippoExpression

  case class Access(target: HippoExpression, name: HippoIdentifier) extends HippoExpression

  case class Apply(target: HippoExpression, args: IndexedSeq[HippoExpression]) extends HippoExpression
  object Apply {
    def apply(target: HippoExpression, args: HippoExpression*): Apply = Apply(target, args.toIndexedSeq)
  }

  case class ApplyTactic(target: HippoExpression, args: IndexedSeq[HippoExpression]) extends HippoExpression
}
