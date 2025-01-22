/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.hippolang.parse.SourceFile
import org.keymaerax.hippolochos.HippoException

import java.io.{PrintWriter, StringWriter}
import scala.annotation.tailrec
import scala.collection.mutable

class HlangException(message: String, cause: Throwable = null)
    extends HippoException(message = message, cause = cause) {

  private val locations: mutable.Buffer[(SourceFile#Slice, String)] = mutable.Buffer.empty

  def addLocation(slice: SourceFile#Slice, label: String = ""): HlangException = {
    locations.append((slice, label))
    this
  }

  private def differingCauseTrace(cause: Throwable): Seq[StackTraceElement] = {
    require(cause != null)

    var ownTrace = getStackTrace.reverseIterator.toList
    var causeTrace = cause.getStackTrace.reverseIterator.toList

    while (ownTrace.nonEmpty && causeTrace.nonEmpty && ownTrace.head == causeTrace.head) {
      ownTrace = ownTrace.tail
      causeTrace = causeTrace.tail
    }

    causeTrace.reverse
  }

  @tailrec
  private def formatCauses(cause: Throwable, blocks: mutable.Buffer[String]): Unit = {
    if (cause == null) return
    val trace = differingCauseTrace(cause).map(_.toString).mkString("\n")
    val block = s"${HlangException.getNameAndMessage(cause)}\n$trace"
    blocks.prepend("Led to:")
    blocks.prepend(block)
    formatCauses(cause.getCause, blocks)
  }

  def format: String = {
    val blocks = mutable.Buffer.empty[String]

    formatCauses(cause, blocks)
    for ((slice, label) <- locations.reverse) blocks.append(slice.format(label))
    blocks.append(s"Error: $message")

    blocks.mkString("\n\n")
  }

  def print(): Unit = println(format)
}

object HlangException {
  private def getNameAndMessage(e: Throwable): String = {
    val name = e.getClass.getName
    Option(e.getMessage) match {
      case Some(msg) => s"$name: ${msg.stripLineEnd}"
      case None => name
    }
  }

  private def getMessageOrName(e: Throwable): String = Option(e.getMessage)
    .map(_.stripLineEnd)
    .getOrElse(e.getClass.getName)

  private def formatCause(e: Throwable): String = {
    val stackTrace = new StringWriter()
    e.printStackTrace(new PrintWriter(stackTrace))
    s"${e.getMessage}\n$stackTrace"
  }

  def apply(message: String, cause: Throwable = null): HlangException = new HlangException(message, cause)

  def apply(message: String, slice: SourceFile#Slice): HlangException = new HlangException(message).addLocation(slice)

  def apply(message: String, slice: SourceFile#Slice, label: String): HlangException = new HlangException(message)
    .addLocation(slice, label)

  def apply(message: String, slice: SourceFile#Slice, label: String, cause: Throwable): HlangException =
    new HlangException(message, cause).addLocation(slice, label)

  @inline
  def at[T](slice: SourceFile#Slice, label: String = "")(body: => T): T =
    try body
    catch {
      case e: HlangException => throw e.addLocation(slice, label)
      case e: Throwable => throw HlangException(getMessageOrName(e), slice, label, cause = e)
    }
}
