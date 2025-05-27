/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.core
import org.keymaerax.hippocore.tools.{Hashable, Hasher}
import org.keymaerax.hippolang.parse.SourceFile

sealed trait HlangExpression extends Hashable {
  def slice: SourceFile#Slice
}

object HlangExpression {
  ///////////////
  // Primitive //
  ///////////////

  case class Const(slice: SourceFile#Slice, value: HlangValue) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  case class DlExpression(slice: SourceFile#Slice, raw: Boolean, value: core.Expression) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(raw).digest(value)
  }

  case class DlSequent(slice: SourceFile#Slice, raw: Boolean, value: core.Sequent) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(raw).digest(value)
  }

  case class Import(slice: SourceFile#Slice, path: HlangExpression) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(path)
  }

  case class Declare(
      slice: SourceFile#Slice,
      exportSlice: Option[SourceFile#Slice],
      mutable: Boolean,
      name: HlangIdentifier,
      value: HlangExpression,
  ) extends HlangExpression {
    def exports: Boolean = exportSlice.isDefined

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(mutable).digest(name).digest(value)
  }

  case class Assign(slice: SourceFile#Slice, name: HlangIdentifier, value: HlangExpression) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(name).digest(value)
  }

  case class AssignGoal(
      slice: SourceFile#Slice,
      nameSlice: SourceFile#Slice,
      name: HlangIdentifier,
      value: HlangExpression,
  ) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(name).digest(value)
  }

  case class Lookup(slice: SourceFile#Slice, name: HlangIdentifier) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(name)
  }

  case class LookupGoal(slice: SourceFile#Slice, name: HlangIdentifier) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(name)
  }

  case class If(
      slice: SourceFile#Slice,
      condition: HlangExpression,
      ifTrue: HlangExpression,
      ifFalse: Option[HlangExpression],
  ) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher
      .digest[this.type]
      .digest(condition)
      .digest(ifTrue)
      .digestOpt(ifFalse)
  }

  case class While(slice: SourceFile#Slice, condition: HlangExpression, body: HlangExpression) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(condition).digest(body)
  }

  case class Match(
      slice: SourceFile#Slice,
      target: HlangExpression,
      cases: Seq[(HlangExpression, HlangExpression)],
      otherwise: Option[HlangExpression],
  ) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher
      .digest[this.type]
      .digest(target)
      .digestSeqWith(cases) { case (hasher, (pattern, body)) => hasher.digest(pattern).digest(body) }
      .digestOpt(otherwise)
  }

  case class Function(slice: SourceFile#Slice, args: Seq[HlangIdentifier], body: HlangExpression)
      extends HlangExpression {

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digestSeq(args).digest(body)
  }

  case class Theorem(
      slice: SourceFile#Slice,
      verifySlice: Option[SourceFile#Slice],
      conclusion: HlangExpression,
      premises: Seq[HlangExpression],
      proof: HlangExpression,
      proofSlice: SourceFile#Slice,
  ) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher
      .digest[this.type]
      .digest(conclusion)
      .digestSeq(premises)
      .digest(proof)
  }

  case class Sequence(slice: SourceFile#Slice, exprs: Seq[HlangExpression], returnExpr: Option[HlangExpression])
      extends HlangExpression {

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digestSeq(exprs).digestOpt(returnExpr)
  }

  case class Block(slice: SourceFile#Slice, inner: HlangExpression) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(inner)
  }

  case class BackwardBlock(
      slice: SourceFile#Slice,
      premises: Seq[HlangIdentifier],
      conclusion: HlangIdentifier,
      inner: HlangExpression,
  ) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher
      .digest[this.type]
      .digestSeq(premises)
      .digest(conclusion)
      .digest(inner)
  }

  case class GraphBlock(slice: SourceFile#Slice, inner: HlangExpression) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(inner)
  }

  ////////////
  // Prefix //
  ////////////

  case class Spread(slice: SourceFile#Slice, target: HlangExpression) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(target)
  }

  ////////////
  // Suffix //
  ////////////

  case class BuiltinAccess(
      slice: SourceFile#Slice,
      target: HlangExpression,
      member: org.keymaerax.hippolang.BuiltinMemberFunction,
  ) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(target).digest(member)
  }

  case class Access(
      slice: SourceFile#Slice,
      nameSlice: SourceFile#Slice,
      target: HlangExpression,
      name: HlangIdentifier,
  ) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(target).digest(name)
  }

  case class Apply(
      slice: SourceFile#Slice,
      target: HlangExpression,
      args: IndexedSeq[HlangExpression],
      argsSlice: SourceFile#Slice,
  ) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(target).digestSeq(args)
  }

  case class ApplyTactic(
      slice: SourceFile#Slice,
      target: HlangExpression,
      args: IndexedSeq[HlangExpression],
      argsSlice: SourceFile#Slice,
  ) extends HlangExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(target).digestSeq(args)
  }
}
