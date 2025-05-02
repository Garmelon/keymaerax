/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.hippocore.tools.{Hashable, Hasher}

sealed abstract class BuiltinFunction(val name: HlangIdentifier, val hidden: Boolean = false) extends Hashable {
  final override def digestInto(hasher: Hasher): Unit = hasher.digest(name)
}

object BuiltinFunction {
  // Arithmetic operations
  case object Not extends BuiltinFunction(HlangIdentifier("not"), hidden = true)
  case object Neg extends BuiltinFunction(HlangIdentifier("neg"), hidden = true)
  case object Mul extends BuiltinFunction(HlangIdentifier("mul"), hidden = true)
  case object Div extends BuiltinFunction(HlangIdentifier("div"), hidden = true)
  case object Add extends BuiltinFunction(HlangIdentifier("add"), hidden = true)
  case object Sub extends BuiltinFunction(HlangIdentifier("sub"), hidden = true)
  case object Gt extends BuiltinFunction(HlangIdentifier("gt"), hidden = true)
  case object Gte extends BuiltinFunction(HlangIdentifier("gte"), hidden = true)
  case object Lt extends BuiltinFunction(HlangIdentifier("lt"), hidden = true)
  case object Lte extends BuiltinFunction(HlangIdentifier("lte"), hidden = true)
  case object Eq extends BuiltinFunction(HlangIdentifier("eq"), hidden = true)
  case object Neq extends BuiltinFunction(HlangIdentifier("neq"), hidden = true)
  case object And extends BuiltinFunction(HlangIdentifier("and"), hidden = true)
  case object Or extends BuiltinFunction(HlangIdentifier("or"), hidden = true)

  // Other operations
  case object List extends BuiltinFunction(HlangIdentifier("list"))
  case object Proof extends BuiltinFunction(HlangIdentifier("proof"))
  case object Print extends BuiltinFunction(HlangIdentifier("print"))
  case object Premise extends BuiltinFunction(HlangIdentifier("premise"))

  val all: Seq[BuiltinFunction] =
    Seq(Not, Neg, Mul, Div, Add, Sub, Gt, Gte, Lt, Lte, Eq, Neq, And, Or, List, Proof, Print, Premise)

  val byName: Map[HlangIdentifier, BuiltinFunction] = all.map(b => b.name -> b).toMap
}
