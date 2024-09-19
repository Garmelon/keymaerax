/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

sealed abstract class BuiltinFunction(val name: HippoIdentifier)

object BuiltinFunction {
  // Arithmetic operations
  case object Not extends BuiltinFunction(HippoIdentifier("not"))
  case object Neg extends BuiltinFunction(HippoIdentifier("neg"))
  case object Mul extends BuiltinFunction(HippoIdentifier("mul"))
  case object Div extends BuiltinFunction(HippoIdentifier("div"))
  case object Add extends BuiltinFunction(HippoIdentifier("add"))
  case object Sub extends BuiltinFunction(HippoIdentifier("sub"))
  case object Gt extends BuiltinFunction(HippoIdentifier("gt"))
  case object Gte extends BuiltinFunction(HippoIdentifier("gte"))
  case object Lt extends BuiltinFunction(HippoIdentifier("lt"))
  case object Lte extends BuiltinFunction(HippoIdentifier("lte"))
  case object Eq extends BuiltinFunction(HippoIdentifier("eq"))
  case object Neq extends BuiltinFunction(HippoIdentifier("neq"))
  case object And extends BuiltinFunction(HippoIdentifier("and"))
  case object Or extends BuiltinFunction(HippoIdentifier("or"))

  // Other operations
  case object List extends BuiltinFunction(HippoIdentifier("list"))
  case object Print extends BuiltinFunction(HippoIdentifier("print"))

  val all: Seq[BuiltinFunction] = Seq(Not, Neg, Mul, Div, Add, Sub, Gt, Gte, Lt, Lte, Eq, Neq, And, Or, List, Print)

  val byName: Map[HippoIdentifier, BuiltinFunction] = all.map(b => b.name -> b).toMap
}
