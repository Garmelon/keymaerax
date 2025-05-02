/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippolang.HlangConversions.*
import org.keymaerax.hippolang.namespace.MutableNamespace
import org.keymaerax.hippolang.{HlangException, HlangExpression, HlangValue}

import java.nio.file.Path

class InterpreterFile(ictx: InterpreterContext, ctx: HippoContext, file: Option[Path])
    extends InterpreterPure(ictx = ictx, ctx = ctx) {

  override def during: String = "during file evaluation"

  val exported = new MutableNamespace()

  override def eval(namespace: MutableNamespace, expr: HlangExpression): HlangValue = expr match {
    case e: HlangExpression.Import =>
      val path = Path.of(eval(namespace, e.path).asString)
      val importFile =
        if (path.isAbsolute) path
        else file.getOrElse(throw new UnsupportedOperationException("code has no path")).getParent.resolve(path)
      HlangException.at(slice = e.slice, label = "while importing this file") { ictx.importCached(importFile).toHValue }

    case e: HlangExpression.Declare if e.exports =>
      val valueV = eval(namespace, e.copy(exportSlice = None))
      exported.declare(e.name, valueV, mutable = true)
      valueV

    case _ => super.eval(namespace, expr)
  }
}
