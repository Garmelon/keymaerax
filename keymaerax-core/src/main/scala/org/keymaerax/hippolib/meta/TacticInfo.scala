/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

import org.keymaerax.hippolochos.Tactic
import org.keymaerax.hippolochos.tools.{Hashable, Hasher}

case class TacticInfo(constructor: TacticConstructor[Tactic]) extends Hashable {
  override def digestInto(hasher: Hasher): Unit = hasher.digest(constructor)
}

object TacticInfo {
  def apply[T <: Tactic](uniqueName: String)(tactic: => T): TacticInfo =
    TacticInfo(constructor = TacticConstructor(uniqueName)(tactic))

  def apply[A1 <: TacticArg, T <: Tactic](uniqueName: String, arg1: TacticArgInfo[A1])(
      build: (arg1.arg.Type) => T
  ): TacticInfo = TacticInfo(constructor = TacticConstructor(uniqueName, arg1)(build))

  def apply[A1 <: TacticArg, A2 <: TacticArg, T <: Tactic](
      uniqueName: String,
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
  )(build: (arg1.arg.Type, arg2.arg.Type) => T): TacticInfo =
    TacticInfo(constructor = TacticConstructor(uniqueName, arg1, arg2)(build))

  def apply[A1 <: TacticArg, A2 <: TacticArg, A3 <: TacticArg, T <: Tactic](
      uniqueName: String,
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      arg3: TacticArgInfo[A3],
  )(build: (arg1.arg.Type, arg2.arg.Type, arg3.arg.Type) => T): TacticInfo =
    TacticInfo(constructor = TacticConstructor(uniqueName, arg1, arg2, arg3)(build))

  def apply[A1 <: TacticArg, A2 <: TacticArg, A3 <: TacticArg, A4 <: TacticArg, T <: Tactic](
      uniqueName: String,
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      arg3: TacticArgInfo[A3],
      arg4: TacticArgInfo[A4],
  )(build: (arg1.arg.Type, arg2.arg.Type, arg3.arg.Type, arg4.arg.Type) => T): TacticInfo =
    TacticInfo(constructor = TacticConstructor(uniqueName, arg1, arg2, arg3, arg4)(build))

  def apply[A1 <: TacticArg, A2 <: TacticArg, A3 <: TacticArg, A4 <: TacticArg, A5 <: TacticArg, T <: Tactic](
      uniqueName: String,
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      arg3: TacticArgInfo[A3],
      arg4: TacticArgInfo[A4],
      arg5: TacticArgInfo[A5],
  )(build: (arg1.arg.Type, arg2.arg.Type, arg3.arg.Type, arg4.arg.Type, arg5.arg.Type) => T): TacticInfo =
    TacticInfo(constructor = TacticConstructor(uniqueName, arg1, arg2, arg3, arg4, arg5)(build))
}
