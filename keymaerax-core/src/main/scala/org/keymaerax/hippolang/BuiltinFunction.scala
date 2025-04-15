/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.hippocore.tools.{Hashable, Hasher}

sealed abstract class BuiltinFunction(val name: HippoIdentifier, val hidden: Boolean = false) extends Hashable {
  final override def digestInto(hasher: Hasher): Unit = hasher.digest(name)
}

object BuiltinFunction {
  // Arithmetic operations
  case object Not extends BuiltinFunction(HippoIdentifier("not"), hidden = true)
  case object Neg extends BuiltinFunction(HippoIdentifier("neg"), hidden = true)
  case object Mul extends BuiltinFunction(HippoIdentifier("mul"), hidden = true)
  case object Div extends BuiltinFunction(HippoIdentifier("div"), hidden = true)
  case object Add extends BuiltinFunction(HippoIdentifier("add"), hidden = true)
  case object Sub extends BuiltinFunction(HippoIdentifier("sub"), hidden = true)
  case object Gt extends BuiltinFunction(HippoIdentifier("gt"), hidden = true)
  case object Gte extends BuiltinFunction(HippoIdentifier("gte"), hidden = true)
  case object Lt extends BuiltinFunction(HippoIdentifier("lt"), hidden = true)
  case object Lte extends BuiltinFunction(HippoIdentifier("lte"), hidden = true)
  case object Eq extends BuiltinFunction(HippoIdentifier("eq"), hidden = true)
  case object Neq extends BuiltinFunction(HippoIdentifier("neq"), hidden = true)
  case object And extends BuiltinFunction(HippoIdentifier("and"), hidden = true)
  case object Or extends BuiltinFunction(HippoIdentifier("or"), hidden = true)

  // Other operations
  case object List extends BuiltinFunction(HippoIdentifier("list"))
  case object Proof extends BuiltinFunction(HippoIdentifier("proof"))
  case object Print extends BuiltinFunction(HippoIdentifier("print"))
  case object Premise extends BuiltinFunction(HippoIdentifier("premise"))

  val all: Seq[BuiltinFunction] =
    Seq(Not, Neg, Mul, Div, Add, Sub, Gt, Gte, Lt, Lte, Eq, Neq, And, Or, List, Proof, Print, Premise)

  val byName: Map[HippoIdentifier, BuiltinFunction] = all.map(b => b.name -> b).toMap
}
