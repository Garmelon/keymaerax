/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.cli

import fastparse.Parsed
import org.keymaerax.hippolochus.HippoDb
import org.keymaerax.hippolochus.interpret.{Namespace, SequentialInterpreter}
import org.keymaerax.hippolochus.parse.{AstExpression, HippoParser}

import java.nio.file.{Files, Path}
import scala.io.StdIn

object CommandHRun {
  def run(in: Option[Path]) = {
    val source = in match {
      case Some(path) => Files.readString(path)
      case None => Iterator.continually(StdIn.readLine()).takeWhile(_ != null).mkString("\n")
    }

    val ast = HippoParser.parse(source) match {
      case success: Parsed.Success[AstExpression] => success.value
      case failure: Parsed.Failure =>
        println(failure.trace().longTerminalsMsg)
        ???
    }

    val program = AstExpression.toHippoExpression(ast)
    val interpreter = SequentialInterpreter(Namespace.populated(HippoDb.filledIn))
    val result = interpreter.eval(program)

    println(result)
  }
}
