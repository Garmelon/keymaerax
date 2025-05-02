/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hashable, Hasher}
import org.keymaerax.hippolang.HlangConversions.*
import org.keymaerax.hippolang.HlangValue
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.parse.{HlangParser, SourceFile}
import org.keymaerax.hippolib.HippoLib

import java.nio.file.{Files, Path}
import scala.collection.mutable

case class InterpreterContext(ctx: HippoContext, env: Option[ImmutableNamespace] = None) extends Hashable {

  private val importCache: mutable.Map[Path, ImmutableNamespace] = mutable.Map.empty

  def run(file: Path): (HlangValue, ImmutableNamespace) = {
    val absFile = file.toAbsolutePath
    val code = Files.readString(absFile)
    run(SourceFile(code, Some(absFile)))
  }

  def run(source: SourceFile): (HlangValue, ImmutableNamespace) = {
    val program = new HlangParser(source).parse().toHExpr
    val fileInterpreter = new InterpreterFile(this, ctx, source.path)
    val value = fileInterpreter.eval(new MutableNamespace(env), program)
    val namespace = fileInterpreter.exported.freeze
    (value, namespace)
  }

  def importCached(file: Path): ImmutableNamespace = importCache.getOrElseUpdate(file.toAbsolutePath, run(file)._2)

  /////////////
  // Hashing //
  /////////////

  lazy val hash: Hash = Hasher().digestOpt(env).hash
  override def digestInto(hasher: Hasher): Unit = hasher.digest(hash)
}

object InterpreterContext {
  def withHippoLib(ctx: HippoContext): InterpreterContext = {
    val lib = new HippoLib()(ctx)
    val env = new EnvBuilder().addBuiltins().addMathKyx().addHippoLib(lib).build()
    InterpreterContext(ctx = ctx, env = Some(env))
  }
}
