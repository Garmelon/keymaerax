/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

import org.keymaerax.hippocore.Tactic
import org.keymaerax.hippocore.tools.{Hashable, Hasher}

case class TacticInfo(constructor: TacticConstructor[Tactic]) extends Hashable {
  override def digestInto(hasher: Hasher): Unit = hasher.digest(constructor)
}

object TacticInfo {
  def arg0[T <: Tactic](uniqueName: String)(tactic: => T): TacticInfo =
    TacticInfo(constructor = TacticConstructor.arg0(uniqueName)(tactic))

  def arg1[A1 <: TacticArg, T <: Tactic](uniqueName: String, arg1: TacticArgInfo[A1], vararg: Boolean = false)(
      build: (arg1.arg.Type) => T
  ): TacticInfo = TacticInfo(constructor = TacticConstructor.arg1(uniqueName, arg1, vararg = vararg)(build))

  def arg2[A1 <: TacticArg, A2 <: TacticArg, T <: Tactic](
      uniqueName: String,
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      vararg: Boolean = false,
  )(build: (arg1.arg.Type, arg2.arg.Type) => T): TacticInfo =
    TacticInfo(constructor = TacticConstructor.arg2(uniqueName, arg1, arg2, vararg = vararg)(build))

  def arg3[A1 <: TacticArg, A2 <: TacticArg, A3 <: TacticArg, T <: Tactic](
      uniqueName: String,
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      arg3: TacticArgInfo[A3],
      vararg: Boolean = false,
  )(build: (arg1.arg.Type, arg2.arg.Type, arg3.arg.Type) => T): TacticInfo =
    TacticInfo(constructor = TacticConstructor.arg3(uniqueName, arg1, arg2, arg3, vararg = vararg)(build))

  def arg4[A1 <: TacticArg, A2 <: TacticArg, A3 <: TacticArg, A4 <: TacticArg, T <: Tactic](
      uniqueName: String,
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      arg3: TacticArgInfo[A3],
      arg4: TacticArgInfo[A4],
      vararg: Boolean = false,
  )(build: (arg1.arg.Type, arg2.arg.Type, arg3.arg.Type, arg4.arg.Type) => T): TacticInfo =
    TacticInfo(constructor = TacticConstructor.arg4(uniqueName, arg1, arg2, arg3, arg4, vararg = vararg)(build))

  def arg5[A1 <: TacticArg, A2 <: TacticArg, A3 <: TacticArg, A4 <: TacticArg, A5 <: TacticArg, T <: Tactic](
      uniqueName: String,
      arg1: TacticArgInfo[A1],
      arg2: TacticArgInfo[A2],
      arg3: TacticArgInfo[A3],
      arg4: TacticArgInfo[A4],
      arg5: TacticArgInfo[A5],
      vararg: Boolean = false,
  )(build: (arg1.arg.Type, arg2.arg.Type, arg3.arg.Type, arg4.arg.Type, arg5.arg.Type) => T): TacticInfo =
    TacticInfo(constructor = TacticConstructor.arg5(uniqueName, arg1, arg2, arg3, arg4, arg5, vararg = vararg)(build))
}
