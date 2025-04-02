/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.core
import org.keymaerax.hippolang.parse.SourceFile
import org.keymaerax.hippolochos.tools.{Hashable, Hasher}

sealed trait HippoExpression extends Hashable {
  def slice: SourceFile#Slice
}

object HippoExpression {
  ///////////////
  // Primitive //
  ///////////////

  case class Const(slice: SourceFile#Slice, value: HippoValue) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  case class DlExpression(slice: SourceFile#Slice, value: core.Expression) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  case class Import(slice: SourceFile#Slice, path: HippoExpression) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(path)
  }

  case class Declare(
      slice: SourceFile#Slice,
      exportSlice: Option[SourceFile#Slice],
      mutable: Boolean,
      name: HippoIdentifier,
      value: HippoExpression,
  ) extends HippoExpression {
    def exports: Boolean = exportSlice.isDefined

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(mutable).digest(name).digest(value)
  }

  case class Assign(slice: SourceFile#Slice, name: HippoIdentifier, value: HippoExpression) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(name).digest(value)
  }

  case class AssignGoal(
      slice: SourceFile#Slice,
      nameSlice: SourceFile#Slice,
      name: HippoIdentifier,
      value: HippoExpression,
  ) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(name).digest(value)
  }

  case class Lookup(slice: SourceFile#Slice, name: HippoIdentifier) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(name)
  }

  case class LookupGoal(slice: SourceFile#Slice, name: HippoIdentifier) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(name)
  }

  case class If(
      slice: SourceFile#Slice,
      condition: HippoExpression,
      ifTrue: HippoExpression,
      ifFalse: Option[HippoExpression],
  ) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher
      .digest[this.type]
      .digest(condition)
      .digest(ifTrue)
      .digestOpt(ifFalse)
  }

  case class While(slice: SourceFile#Slice, condition: HippoExpression, body: HippoExpression) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(condition).digest(body)
  }

  case class Function(slice: SourceFile#Slice, args: Seq[HippoIdentifier], body: HippoExpression)
      extends HippoExpression {

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digestSeq(args).digest(body)
  }

  case class Theorem(
      slice: SourceFile#Slice,
      verifySlice: Option[SourceFile#Slice],
      conclusion: HippoExpression,
      premises: Seq[HippoExpression],
      proof: HippoExpression,
      proofSlice: SourceFile#Slice,
  ) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher
      .digest[this.type]
      .digest(conclusion)
      .digestSeq(premises)
      .digest(proof)
  }

  case class Sequence(slice: SourceFile#Slice, exprs: Seq[HippoExpression], returnExpr: Option[HippoExpression])
      extends HippoExpression {

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digestSeq(exprs).digestOpt(returnExpr)
  }

  case class Block(slice: SourceFile#Slice, inner: HippoExpression) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(inner)
  }

  case class BackwardBlock(
      slice: SourceFile#Slice,
      premises: Seq[HippoIdentifier],
      conclusion: HippoIdentifier,
      inner: HippoExpression,
  ) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher
      .digest[this.type]
      .digestSeq(premises)
      .digest(conclusion)
      .digest(inner)
  }

  case class GraphBlock(slice: SourceFile#Slice, inner: HippoExpression) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(inner)
  }

  ////////////
  // Suffix //
  ////////////

  case class BuiltinAccess(
      slice: SourceFile#Slice,
      target: HippoExpression,
      member: org.keymaerax.hippolang.BuiltinMemberFunction,
  ) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(target).digest(member)
  }

  case class Access(slice: SourceFile#Slice, target: HippoExpression, name: HippoIdentifier) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(target).digest(name)
  }

  case class Apply(
      slice: SourceFile#Slice,
      target: HippoExpression,
      args: IndexedSeq[HippoExpression],
      argsSlice: SourceFile#Slice,
  ) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(target).digestSeq(args)
  }

  case class ApplyTactic(
      slice: SourceFile#Slice,
      target: HippoExpression,
      args: IndexedSeq[HippoExpression],
      argsSlice: SourceFile#Slice,
  ) extends HippoExpression {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(target).digestSeq(args)
  }
}
