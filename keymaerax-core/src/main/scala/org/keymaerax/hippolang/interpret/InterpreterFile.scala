/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippolang.HippoConversions.*
import org.keymaerax.hippolang.namespace.MutableNamespace
import org.keymaerax.hippolang.{HippoExpression, HippoValue, HlangException}
import org.keymaerax.hippolochos.run.HippoContext

import java.nio.file.Path

class InterpreterFile(ictx: HippoInterpreterContext, ctx: HippoContext, file: Option[Path])
    extends InterpreterPure(ictx = ictx, ctx = ctx) {

  val exported = new MutableNamespace()

  override def eval(namespace: MutableNamespace, expr: HippoExpression): HippoValue = expr match {
    case e: HippoExpression.Import =>
      val path = Path.of(eval(namespace, e.path).asString)
      val importFile =
        if (path.isAbsolute) path
        else file.getOrElse(throw new UnsupportedOperationException("code has no path")).getParent.resolve(path)
      HlangException.at(slice = e.slice, label = "while importing this file") { ictx.importCached(importFile).toHValue }

    case e: HippoExpression.Declare if e.exports =>
      val valueV = eval(namespace, e.copy(exportSlice = None))
      exported.declare(e.name, valueV, mutable = true)
      valueV

    case _ => super.eval(namespace, expr)
  }
}
