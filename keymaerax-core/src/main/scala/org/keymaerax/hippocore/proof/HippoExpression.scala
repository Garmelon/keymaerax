/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.proof

import org.keymaerax.core.Expression
import org.keymaerax.hippocore.definitions.{Definitions, Name}
import org.keymaerax.hippocore.tools.{Hashable, Hasher}

case class HippoExpression private (expr: Expression, defs: Definitions) extends Hashable {
  def exprExpanded: Expression = defs.expandAll(expr)

  def expand(name: Name): HippoExpression = HippoExpression(defs.expand(name, expr), defs)
  def expandAll: HippoExpression = HippoExpression(defs.expandAll(expr), defs)

  override def digestInto(hasher: Hasher): Unit = hasher.digest(expr).digest(defs)
}

object HippoExpression {
  def apply(expr: Expression, defs: Definitions = Definitions.empty): HippoExpression =
    new HippoExpression(expr = expr, defs = defs.retainReachable(expr))
}
