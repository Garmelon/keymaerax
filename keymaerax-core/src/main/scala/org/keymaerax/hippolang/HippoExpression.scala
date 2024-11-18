/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.hippolang.parse.SourceFile

sealed trait HippoExpression {
  def slice: SourceFile#Slice
}

object HippoExpression {
  ///////////////
  // Primitive //
  ///////////////

  case class Const(slice: SourceFile#Slice, value: HippoValue) extends HippoExpression

  case class Import(slice: SourceFile#Slice, path: HippoExpression) extends HippoExpression

  case class Declare(
      slice: SourceFile#Slice,
      exportSlice: Option[SourceFile#Slice],
      mutable: Boolean,
      name: HippoIdentifier,
      value: HippoExpression,
  ) extends HippoExpression {
    def exports: Boolean = exportSlice.isDefined
  }

  case class Assign(slice: SourceFile#Slice, name: HippoIdentifier, value: HippoExpression) extends HippoExpression

  case class Lookup(slice: SourceFile#Slice, name: HippoIdentifier) extends HippoExpression

  case class If(
      slice: SourceFile#Slice,
      condition: HippoExpression,
      ifTrue: HippoExpression,
      ifFalse: Option[HippoExpression],
  ) extends HippoExpression

  case class While(slice: SourceFile#Slice, condition: HippoExpression, body: HippoExpression) extends HippoExpression

  case class Function(slice: SourceFile#Slice, args: Seq[HippoIdentifier], body: HippoExpression)
      extends HippoExpression

  case class Theorem(
      slice: SourceFile#Slice,
      verifySlice: Option[SourceFile#Slice],
      conclusion: HippoExpression,
      premises: Seq[HippoExpression],
      proof: HippoExpression,
      proofSlice: SourceFile#Slice,
  ) extends HippoExpression

  case class Sequence(slice: SourceFile#Slice, exprs: Seq[HippoExpression], returnExpr: Option[HippoExpression])
      extends HippoExpression

  case class Block(slice: SourceFile#Slice, inner: HippoExpression) extends HippoExpression

  case class BackwardBlock(slice: SourceFile#Slice, inner: HippoExpression) extends HippoExpression

  case class GraphBlock(slice: SourceFile#Slice, inner: HippoExpression) extends HippoExpression

  ////////////
  // Suffix //
  ////////////

  case class BuiltinAccess(
      slice: SourceFile#Slice,
      target: HippoExpression,
      member: org.keymaerax.hippolang.BuiltinMemberFunction,
  ) extends HippoExpression

  case class Access(slice: SourceFile#Slice, target: HippoExpression, name: HippoIdentifier) extends HippoExpression

  case class Apply(
      slice: SourceFile#Slice,
      target: HippoExpression,
      args: IndexedSeq[HippoExpression],
      argsSlice: SourceFile#Slice,
  ) extends HippoExpression

  case class ApplyTactic(
      slice: SourceFile#Slice,
      target: HippoExpression,
      args: IndexedSeq[HippoExpression],
      argsSlice: SourceFile#Slice,
  ) extends HippoExpression
}
