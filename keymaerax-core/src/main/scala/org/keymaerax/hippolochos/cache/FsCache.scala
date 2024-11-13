/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.cache

import org.keymaerax.hippolochos.tools.Hash

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, NoSuchFileException, Path, SimpleFileVisitor, StandardOpenOption}
import scala.util.Try

abstract class FsCache[V](dir: Path, version: Int) extends Cache[V] {
  private val versionPath = dir.resolve("version")
  private val prevVersion = Try { Files.readString(versionPath).trim.toInt }.toOption
  if (!prevVersion.contains(version)) clear()
  Files.createDirectories(dir)
  Files.writeString(versionPath, s"$version\n")

  def format(value: V): String

  def parse(str: String): V

  private def pathForKey(key: Hash): Path = dir.resolve(key.hexString.slice(0, 2)).resolve(key.hexString)

  override def get(key: Hash): Option[V] = Try { Files.readString(pathForKey(key)) }.toOption.map(parse)

  override def put(key: Hash, value: V): Unit = {
    val path = pathForKey(key)
    val str = format(value)

    Files.createDirectories(path.getParent)
    Files.writeString(path, str, StandardOpenOption.CREATE)
  }

  override def clear(): Unit = Files.walkFileTree(
    dir,
    new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }

      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = exc match {
        case _: NoSuchFileException => FileVisitResult.CONTINUE
        case _ => throw exc
      }
    },
  )
}
