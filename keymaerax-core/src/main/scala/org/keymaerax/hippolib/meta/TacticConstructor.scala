/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

import org.keymaerax.hippolochos.Tactic
import org.keymaerax.hippolochos.run.HippoContext

trait TacticConstructor[+T <: Tactic] {
  val args: IndexedSeq[TacticArgInfo[TacticArg]]
  def construct(ctx: HippoContext, args: Seq[Any]): T

  final def constructPositional(ctx: HippoContext, args: Seq[Any]): T = {
    require(args.length <= this.args.length)

    val defaultArgs = this
      .args
      .takeRight(this.args.length - args.length)
      .map(_.getDefault(ctx).getOrElse(throw new Exception("default values required")))

    construct(ctx, args ++ defaultArgs)
  }

  final def constructNamed(ctx: HippoContext, args: Map[String, Any]): T = {
    val argsSeq = for (arg <- this.args)
      yield args.get(arg.name).orElse(arg.getDefault(ctx)).getOrElse(throw new Exception("arg or default value required"))

    construct(ctx, argsSeq)
  }
}

// Originally, I tried to create case classes for every constructor in order to retain arg info in the static types.
// However, the Scala 2 compiler couldn't cope with the necessary type magic, which only works on functions/methods.
// Fixed in Scala 3, see https://github.com/scala/bug/issues/5712
//
// The kind of class I was going for:
//
// case class TacticConstructor2[A1 <: TacticArg, A2 <: TacticArg,T <: Tactic](arg1: A1, arg2: A2)(
//   build: (arg1.Type, arg2.Type, ...) => T) extends TacticConstructor[T] { ... }

object TacticConstructor {
  def apply[T <: Tactic](tactic: T): TacticConstructor[T] = new TacticConstructor[T] {
    override val args: IndexedSeq[TacticArgInfo[TacticArg]] = IndexedSeq()
    override def construct(ctx: HippoContext, args: Seq[Any]): T = {
      val Seq() = args
      tactic
    }
  }

  def apply[A1 <: TacticArg, T <: Tactic](arg1: TacticArgInfo[A1])(build: arg1.arg.Type => T): TacticConstructor[T] =
    new TacticConstructor[T] {
      override val args: IndexedSeq[TacticArgInfo[TacticArg]] = IndexedSeq(arg1)
      override def construct(ctx: HippoContext, args: Seq[Any]): T = {
        val Seq(val1) = args
        build(arg1.arg.validate(ctx, val1))
      }
    }

  def apply[A1 <: TacticArg, A2 <: TacticArg, T <: Tactic](arg1: TacticArgInfo[A1], arg2: TacticArgInfo[A2])(
      build: (arg1.arg.Type, arg2.arg.Type) => T
  ): TacticConstructor[T] = new TacticConstructor[T] {
    override val args: IndexedSeq[TacticArgInfo[TacticArg]] = IndexedSeq(arg1, arg2)
    override def construct(ctx: HippoContext, args: Seq[Any]): T = {
      val Seq(val1, val2) = args
      build(arg1.arg.validate(ctx, val1), arg2.arg.validate(ctx, val2))
    }
  }

  def apply[A1 <: TacticArg, A2 <: TacticArg, A3 <: TacticArg, T <: Tactic](
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      arg3: TacticArgInfo[A3],
  )(build: (arg1.arg.Type, arg2.arg.Type, arg3.arg.Type) => T): TacticConstructor[T] = new TacticConstructor[T] {
    override val args: IndexedSeq[TacticArgInfo[TacticArg]] = IndexedSeq(arg1, arg2, arg3)
    override def construct(ctx: HippoContext, args: Seq[Any]): T = {
      val Seq(val1, val2, val3) = args
      build(arg1.arg.validate(ctx, val1), arg2.arg.validate(ctx, val2), arg3.arg.validate(ctx, val3))
    }
  }

  def apply[A1 <: TacticArg, A2 <: TacticArg, A3 <: TacticArg, A4 <: TacticArg, T <: Tactic](
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      arg3: TacticArgInfo[A3],
      arg4: TacticArgInfo[A4],
  )(build: (arg1.arg.Type, arg2.arg.Type, arg3.arg.Type, arg4.arg.Type) => T): TacticConstructor[T] =
    new TacticConstructor[T] {
      override val args: IndexedSeq[TacticArgInfo[TacticArg]] = IndexedSeq(arg1, arg2, arg3, arg4)
      override def construct(ctx: HippoContext, args: Seq[Any]): T = {
        val Seq(val1, val2, val3, val4) = args
        build(
          arg1.arg.validate(ctx, val1),
          arg2.arg.validate(ctx, val2),
          arg3.arg.validate(ctx, val3),
          arg4.arg.validate(ctx, val4),
        )
      }
    }

  def apply[A1 <: TacticArg, A2 <: TacticArg, A3 <: TacticArg, A4 <: TacticArg, A5 <: TacticArg, T <: Tactic](
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      arg3: TacticArgInfo[A3],
      arg4: TacticArgInfo[A4],
      arg5: TacticArgInfo[A5],
  )(build: (arg1.arg.Type, arg2.arg.Type, arg3.arg.Type, arg4.arg.Type, arg5.arg.Type) => T): TacticConstructor[T] =
    new TacticConstructor[T] {
      override val args: IndexedSeq[TacticArgInfo[TacticArg]] = IndexedSeq(arg1, arg2, arg3, arg4, arg5)
      override def construct(ctx: HippoContext, args: Seq[Any]): T = {
        val Seq(val1, val2, val3, val4, val5) = args
        build(
          arg1.arg.validate(ctx, val1),
          arg2.arg.validate(ctx, val2),
          arg3.arg.validate(ctx, val3),
          arg4.arg.validate(ctx, val4),
          arg5.arg.validate(ctx, val5),
        )
      }
    }
}
