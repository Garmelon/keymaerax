/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippolang.HippoConversions._
import org.keymaerax.hippolang.HippoValue
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.parse.{HippoParser, SourceFile}
import org.keymaerax.hippolochos.run.HippoContext

import java.nio.file.{Files, Path}

case class HippoInterpreterContext(ctx: HippoContext, env: Option[ImmutableNamespace] = None) {
  def run(file: Path): (HippoValue, ImmutableNamespace) = {
    val absFile = file.toAbsolutePath
    val code = Files.readString(absFile)
    run(SourceFile(code, Some(absFile)))
  }

  def run(source: SourceFile): (HippoValue, ImmutableNamespace) = {
    val program = HippoParser.parse(source).toHExpr
    val fileInterpreter = new FileInterpreter(this, ctx, source.path)
    val value = fileInterpreter.eval(new MutableNamespace(env), program)
    val namespace = fileInterpreter.exported.freeze
    (value, namespace)
  }
}
