/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

import org.keymaerax.hippolochos.tools.{Hash, Hashable, Hasher, UniqueNameRegistry}
import org.keymaerax.hippolochos.{HippoException, Tactic}

trait TacticConstructor[+T <: Tactic] extends Hashable {
  def hash: Hash
  final override def digestInto(hasher: Hasher): Unit = hasher.digest(hash)

  val args: IndexedSeq[TacticArgInfo[TacticArg]]
  lazy val argNames: Set[String] = args.map(_.name).toSet // Defer initialization until after object exists
  def construct(values: Seq[Any]): T

  final def constructPositional(values: IndexedSeq[Any]): T = {
    HippoException.require(
      values.length <= args.length,
      s"Too many arguments: Expected exactly ${args.length}, but got ${values.length}",
    )

    val allArgs = args
      .zipWithIndex
      .map {
        case (_, i) if i < values.length => values(i)
        case (arg, i) => arg
            .getDefault
            .getOrElse(HippoException.fail(
              s"Missing argument ${i + 1} named ${arg.name}: No value was provided and no default value exists"
            ))
      }

    construct(allArgs)
  }

  final def constructNamed(values: Map[String, Any]): T = {
    for ((name, _) <- values) HippoException
      .require(argNames.contains(name), s"Too many arguments: Expected no argument named $name")

    val allArgs = args
      .zipWithIndex
      .map { case (arg, i) =>
        values
          .get(arg.name)
          .orElse(arg.getDefault)
          .getOrElse(HippoException.fail(
            s"Missing argument ${i + 1} named ${arg.name}: No value was provided and no default value exists"
          ))
      }

    construct(allArgs)
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
  def apply[T <: Tactic](uniqueName: String)(tactic: => T)(implicit names: UniqueNameRegistry): TacticConstructor[T] =
    new TacticConstructor[T] {
      names.register(uniqueName)
      override val hash: Hash = Hasher().digest(uniqueName).hash
      override val args: IndexedSeq[TacticArgInfo[TacticArg]] = IndexedSeq()
      override def construct(values: Seq[Any]): T = {
        val Seq() = values
        tactic
      }
    }

  def apply[A1 <: TacticArg, T <: Tactic](uniqueName: String, arg1: TacticArgInfo[A1])(
      build: arg1.arg.Type => T
  )(implicit names: UniqueNameRegistry): TacticConstructor[T] = new TacticConstructor[T] {
    names.register(uniqueName)
    override val hash: Hash = Hasher().digest(uniqueName).hash
    override val args: IndexedSeq[TacticArgInfo[TacticArg]] = IndexedSeq(arg1)
    override def construct(values: Seq[Any]): T = {
      val Seq(val1) = values
      build(arg1.arg.validate(val1))
    }
  }

  def apply[A1 <: TacticArg, A2 <: TacticArg, T <: Tactic](
      uniqueName: String,
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
  )(build: (arg1.arg.Type, arg2.arg.Type) => T)(implicit names: UniqueNameRegistry): TacticConstructor[T] =
    new TacticConstructor[T] {
      names.register(uniqueName)
      override val hash: Hash = Hasher().digest(uniqueName).hash
      override val args: IndexedSeq[TacticArgInfo[TacticArg]] = IndexedSeq(arg1, arg2)
      override def construct(values: Seq[Any]): T = {
        val Seq(val1, val2) = values
        build(arg1.arg.validate(val1), arg2.arg.validate(val2))
      }
    }

  def apply[A1 <: TacticArg, A2 <: TacticArg, A3 <: TacticArg, T <: Tactic](
      uniqueName: String,
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      arg3: TacticArgInfo[A3],
  )(
      build: (arg1.arg.Type, arg2.arg.Type, arg3.arg.Type) => T
  )(implicit names: UniqueNameRegistry): TacticConstructor[T] = new TacticConstructor[T] {
    names.register(uniqueName)
    override val hash: Hash = Hasher().digest(uniqueName).hash
    override val args: IndexedSeq[TacticArgInfo[TacticArg]] = IndexedSeq(arg1, arg2, arg3)
    override def construct(values: Seq[Any]): T = {
      val Seq(val1, val2, val3) = values
      build(arg1.arg.validate(val1), arg2.arg.validate(val2), arg3.arg.validate(val3))
    }
  }

  def apply[A1 <: TacticArg, A2 <: TacticArg, A3 <: TacticArg, A4 <: TacticArg, T <: Tactic](
      uniqueName: String,
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      arg3: TacticArgInfo[A3],
      arg4: TacticArgInfo[A4],
  )(
      build: (arg1.arg.Type, arg2.arg.Type, arg3.arg.Type, arg4.arg.Type) => T
  )(implicit names: UniqueNameRegistry): TacticConstructor[T] = new TacticConstructor[T] {
    names.register(uniqueName)
    override val hash: Hash = Hasher().digest(uniqueName).hash
    override val args: IndexedSeq[TacticArgInfo[TacticArg]] = IndexedSeq(arg1, arg2, arg3, arg4)
    override def construct(values: Seq[Any]): T = {
      val Seq(val1, val2, val3, val4) = values
      build(arg1.arg.validate(val1), arg2.arg.validate(val2), arg3.arg.validate(val3), arg4.arg.validate(val4))
    }
  }

  def apply[A1 <: TacticArg, A2 <: TacticArg, A3 <: TacticArg, A4 <: TacticArg, A5 <: TacticArg, T <: Tactic](
      uniqueName: String,
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      arg3: TacticArgInfo[A3],
      arg4: TacticArgInfo[A4],
      arg5: TacticArgInfo[A5],
  )(
      build: (arg1.arg.Type, arg2.arg.Type, arg3.arg.Type, arg4.arg.Type, arg5.arg.Type) => T
  )(implicit names: UniqueNameRegistry): TacticConstructor[T] = new TacticConstructor[T] {
    names.register(uniqueName)
    override val hash: Hash = Hasher().digest(uniqueName).hash
    override val args: IndexedSeq[TacticArgInfo[TacticArg]] = IndexedSeq(arg1, arg2, arg3, arg4, arg5)
    override def construct(values: Seq[Any]): T = {
      val Seq(val1, val2, val3, val4, val5) = values
      build(
        arg1.arg.validate(val1),
        arg2.arg.validate(val2),
        arg3.arg.validate(val3),
        arg4.arg.validate(val4),
        arg5.arg.validate(val5),
      )
    }
  }
}
