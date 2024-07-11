/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus

sealed abstract class HippoBuiltin(val name: HippoIdentifier)

object HippoBuiltin {
  case object Not extends HippoBuiltin(HippoIdentifier("not"))
  case object Neg extends HippoBuiltin(HippoIdentifier("neg"))
  case object Mul extends HippoBuiltin(HippoIdentifier("mul"))
  case object Div extends HippoBuiltin(HippoIdentifier("div"))
  case object Add extends HippoBuiltin(HippoIdentifier("add"))
  case object Sub extends HippoBuiltin(HippoIdentifier("sub"))
  case object Gt extends HippoBuiltin(HippoIdentifier("gt"))
  case object Gte extends HippoBuiltin(HippoIdentifier("gte"))
  case object Lt extends HippoBuiltin(HippoIdentifier("lt"))
  case object Lte extends HippoBuiltin(HippoIdentifier("lte"))
  case object Eq extends HippoBuiltin(HippoIdentifier("eq"))
  case object Neq extends HippoBuiltin(HippoIdentifier("neq"))
  case object And extends HippoBuiltin(HippoIdentifier("and"))
  case object Or extends HippoBuiltin(HippoIdentifier("or"))

  case object Print extends HippoBuiltin(HippoIdentifier("print"))

  val all: Seq[HippoBuiltin] = Seq(Not, Neg, Mul, Div, Add, Sub, Gt, Gte, Lt, Lte, Eq, Neq, And, Or, Print)

  val byName: Map[HippoIdentifier, HippoBuiltin] = all.map(b => b.name -> b).toMap
}
