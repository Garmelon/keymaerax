/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.hippolang.parse.SourceFile
import org.keymaerax.hippolochos.HippoException

import java.io.{PrintWriter, StringWriter}
import scala.collection.mutable

class HlangException(message: String, cause: Throwable = null)
    extends HippoException(message = message, cause = cause) {

  private val locations: mutable.Buffer[(SourceFile#Slice, String)] = mutable.Buffer.empty

  def addLocation(slice: SourceFile#Slice, label: String = ""): HlangException = {
    locations.append((slice, label))
    this
  }

  def formatForwards: String = {
    val blocks = mutable.Buffer.empty[String]

    blocks.append(s"Error: $message")
    for ((slice, label) <- locations) blocks.append(slice.format(label))
    if (cause != null) blocks.append(HlangException.formatCause(cause))

    blocks.mkString("\n\n")
  }

  def formatBackwards: String = {
    val blocks = mutable.Buffer.empty[String]

    if (cause != null) blocks.append(HlangException.formatCause(cause))
    for ((slice, label) <- locations.reverse) blocks.append(slice.format(label))
    blocks.append(s"Error: $message")

    blocks.mkString("\n\n")
  }

  def printForwards(): Unit = println(formatForwards)
  def printBackwards(): Unit = println(formatBackwards)
}

object HlangException {
  private def formatCause(e: Throwable): String = {
    val stackTrace = new StringWriter()
    e.printStackTrace(new PrintWriter(stackTrace))
    s"${e.getMessage}\n$stackTrace"
  }

  def apply(message: String, cause: Throwable = null): HlangException = new HlangException(message, cause)

  def apply(message: String, slice: SourceFile#Slice, label: String): HlangException = new HlangException(message)
    .addLocation(slice, label)

  def apply(message: String, slice: SourceFile#Slice): HlangException = new HlangException(message).addLocation(slice)

  def apply(message: String, slice: SourceFile#Slice, label: String, cause: Throwable): HlangException =
    new HlangException(message, cause).addLocation(slice, label)

  @inline
  def at[T](slice: SourceFile#Slice, label: String)(body: => T): T =
    try body
    catch {
      case e: HlangException => throw e.addLocation(slice, label)
      case e: Throwable => throw HlangException(e.getMessage, slice, label, cause = e)
    }
}
