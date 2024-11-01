/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

case class TacticArgInfo[+A <: TacticArg](name: String, arg: A, description: Option[String], default: Option[Any]) {
  def getDefault: Option[arg.Type] = default.map(arg.validate)
}

object TacticArgInfo {
  def apply[A <: TacticArg](
      name: String,
      arg: A,
      description: String = "",
      default: Option[Any] = None,
  ): TacticArgInfo[A] = TacticArgInfo(
    name = name.trim,
    arg = arg,
    description = Some(description.trim).filter(_.nonEmpty),
    default = default,
  )
}
