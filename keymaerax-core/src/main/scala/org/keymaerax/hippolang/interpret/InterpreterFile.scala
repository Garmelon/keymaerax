/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippolang.HippoConversions._
import org.keymaerax.hippolang.namespace.MutableNamespace
import org.keymaerax.hippolang.{HippoExpression, HippoValue}
import org.keymaerax.hippolochos.run.HippoContext

import java.nio.file.Path

class InterpreterFile(ictx: HippoInterpreterContext, ctx: HippoContext, file: Option[Path])
    extends InterpreterPure(ictx = ictx, ctx = ctx) {

  val exported = new MutableNamespace()

  override def eval(namespace: MutableNamespace, expr: HippoExpression): HippoValue = expr match {
    case HippoExpression.Import(pathE) =>
      // TODO Cache imported files by path (assuming a file never changes during the existence of an interpreter)
      val pathV = Path.of(eval(namespace, pathE).asString)
      val importFile =
        if (pathV.isAbsolute) pathV
        else file.getOrElse(throw new UnsupportedOperationException("code has no path")).getParent.resolve(pathV)
      val (_, importNamespace) = ictx.run(importFile)
      importNamespace.toHValue

    case HippoExpression.Declare(true, mutable, name, value) =>
      val valueV = eval(namespace, HippoExpression.Declare(exports = false, mutable, name, value))
      exported.declare(name, valueV, mutable = true)
      valueV

    case _ => super.eval(namespace, expr)
  }
}
