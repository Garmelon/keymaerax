/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.cli

import org.keymaerax.FileConfiguration
import org.keymaerax.btactics.ToolProvider
import org.keymaerax.cli.KeymaeraxCore.{exit, initializeBackend}
import org.keymaerax.hippolang.interpret.HippoInterpreterContext
import org.keymaerax.hippolang.parse.HlParseError
import org.keymaerax.hippolochos.run.HippoContext

import java.nio.file.Path

/** Initialize a context in which hippolang programs can be executed, and implement hippolang-related CLI programs. */
class Hippo(options: Options) {
  initializeBackend(options.toToolConfig)
  private val home = Path.of(FileConfiguration.KEYMAERAX_HOME_PATH)
  private val cacheDir = home.resolve("hippo").resolve("cache")
  private val ctx = HippoContext.withCacheDir(ToolProvider.provider, cacheDir)
  private val interpreter = HippoInterpreterContext.withHippoLib(ctx)

  def run(file: Path): Unit = {
    try interpreter.run(file)
    catch {
      case e: HlParseError =>
        e.print()
        exit(1)
      case e: Exception =>
        println("An exception occurred during hippo evaluation:")
        println(e.getMessage)
        e.printStackTrace()
        exit(1)
    }
  }
}
