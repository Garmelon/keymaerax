/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

sealed abstract class BuiltinMemberFunction(val name: HippoIdentifier) {}

object BuiltinMemberFunction {
  case object Forward extends BuiltinMemberFunction(HippoIdentifier("forward"))
  case object Backward extends BuiltinMemberFunction(HippoIdentifier("backward"))
  case object Pure extends BuiltinMemberFunction(HippoIdentifier("pure"))
  case object Join extends BuiltinMemberFunction(HippoIdentifier("join"))
  case object Usubst extends BuiltinMemberFunction(HippoIdentifier("usubst"))
  case object Select extends BuiltinMemberFunction(HippoIdentifier("select"))

  val all: Seq[BuiltinMemberFunction] = Seq(Forward, Backward, Pure, Join, Usubst, Select)

  val byName: Map[HippoIdentifier, BuiltinMemberFunction] = all.map(b => b.name -> b).toMap
}
