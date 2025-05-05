/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.hippocore.tools.{Hashable, Hasher}

sealed abstract class BuiltinMemberFunction(val name: HlangIdentifier) extends Hashable {
  override def digestInto(hasher: Hasher): Unit = hasher.digest(name)
}

object BuiltinMemberFunction {
  case object Forward extends BuiltinMemberFunction(HlangIdentifier("forward"))
  case object Backward extends BuiltinMemberFunction(HlangIdentifier("backward"))
  case object Pure extends BuiltinMemberFunction(HlangIdentifier("pure"))
  case object Join extends BuiltinMemberFunction(HlangIdentifier("join"))
  case object Usubst extends BuiltinMemberFunction(HlangIdentifier("usubst"))
  case object Urename extends BuiltinMemberFunction(HlangIdentifier("urename"))
  case object Select extends BuiltinMemberFunction(HlangIdentifier("select"))
  case object Expand extends BuiltinMemberFunction(HlangIdentifier("expand"))
  case object ExpandAll extends BuiltinMemberFunction(HlangIdentifier("expandAll"))

  val all: Seq[BuiltinMemberFunction] = Seq(Forward, Backward, Pure, Join, Usubst, Urename, Select, Expand, ExpandAll)

  val byName: Map[HlangIdentifier, BuiltinMemberFunction] = all.map(b => b.name -> b).toMap
}
