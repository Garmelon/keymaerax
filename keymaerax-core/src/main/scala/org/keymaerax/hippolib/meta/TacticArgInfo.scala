/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

import org.keymaerax.hippolochos.run.HippoContext

case class TacticArgInfo[+A <: TacticArg](name: String, arg: A, description: Option[String], default: Option[Any]) {
  def getDefault(ctx: HippoContext): Option[arg.Type] = default.map(arg.validate(ctx, _))
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
