/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.cli

import org.keymaerax.FileConfiguration
import org.keymaerax.btactics.ToolProvider
import org.keymaerax.cli.KeymaeraxCore.{combineToolConfigs, exit, initializeProver, toolConfigFromFile}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippolang.HlangException
import org.keymaerax.hippolang.interpret.InterpreterContext
import org.keymaerax.tools.ToolName

import java.nio.file.{FileSystems, Path, StandardWatchEventKinds}
import java.util.concurrent.TimeUnit
import scala.util.control.Breaks.breakable

/** Initialize a context in which hippolang programs can be executed, and implement hippolang-related CLI programs. */
class Hippo(options: Options) {
  // Since we support using bellerophon tactics from hippolochos, we need to do a slow, full initialization.
  initializeProver(combineToolConfigs(options.toToolConfig, toolConfigFromFile(ToolName.Z3)))

  private val home = Path.of(FileConfiguration.KEYMAERAX_HOME_PATH)
  private val cacheDir = home.resolve("hippo").resolve("cache")
  private val ctx = HippoContext.withCacheDir(ToolProvider.provider, cacheDir)
  private val interpreter = InterpreterContext.withHippoLib(ctx)

  def run(file: Path): Unit = {
    try interpreter.run(file)
    catch {
      case e: HlangException =>
        println()
        e.print()
        exit(1)
      case e: Throwable =>
        println("An exception occurred during hippo evaluation:")
        println(e.getMessage)
        e.printStackTrace()
        exit(1)
    }
  }

  def runOrPrintError(file: Path): Unit = {
    try interpreter.run(file)
    catch {
      case e: HlangException =>
        println()
        e.print()
      case e: Throwable =>
        println("An exception occurred during hippo evaluation:")
        println(e.getMessage)
        e.printStackTrace()
    }
  }

  def watch(file: Path): Unit = {
    import scala.jdk.CollectionConverters.*

    val watchService = FileSystems.getDefault.newWatchService()
    file
      .getParent
      .register(
        watchService,
        StandardWatchEventKinds.ENTRY_CREATE,
        StandardWatchEventKinds.ENTRY_DELETE,
        StandardWatchEventKinds.ENTRY_MODIFY,
      )

    runOrPrintError(file)

    while (true) breakable {
      var fileChanged = false
      var key = watchService.take()
      while (key != null) {
        val events = key.pollEvents()
        key.reset()
        key = null

        fileChanged = fileChanged ||
          events
            .asScala
            .exists { event =>
              // This conversion is fine because of the events we registered earlier.
              val changedFile = event.context().asInstanceOf[Path]
              changedFile == file.getFileName
            }

        key = watchService.poll(50, TimeUnit.MILLISECONDS)
      }

      if (fileChanged) {
        println()
        println("#######################################")
        println("## File was changed, re-executing... ##")
        println("#######################################")
        println()
        runOrPrintError(file)
      }
    }
  }
}
