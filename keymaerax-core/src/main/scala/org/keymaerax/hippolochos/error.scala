/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos

import org.keymaerax.hippolochos.HippoException.walker
import org.keymaerax.hippolochos.run.{HippoContext, ProofChain}

import java.lang.StackWalker.StackFrame
import scala.collection.mutable
import scala.jdk.StreamConverters._

class HippoException(message: String, cause: Throwable = null) extends Exception(message, cause) {

  private val trace = walker
    .walk[Seq[StackFrame]](stream => stream.toScala(Seq))
    .reverseIterator
    .zipWithIndex
    .filter { case (elem, _) => HippoException.isInterestingClass(elem.getDeclaringClass) }
    .map { case (elem, i) => (i, mutable.Buffer(s"at $elem")) }
    .to(mutable.SortedMap)

  /**
   * This method is safe to use in the following cases:
   *   1. Immediately after constructing the exception
   *   1. when re-throwing immediately after catching
   *
   * If used in other situations, it might get the call stack depth wrong.
   *
   * {{{
   *   new HippoException(...).annotate(...)
   * }}}
   *
   * {{{
   *   try ...
   *   catch {
   *     case e: HippoException => throw e.annotate(...)
   *   }
   * }}}
   */
  def annotate(info: String): this.type = {
    val index = walker.walk[Long](_.count()).toInt - 2
    trace.getOrElseUpdate(index, mutable.Buffer.empty).addOne(info)
    this
  }

  def formatTrace: String = {
    val lines = mutable.Buffer.empty[String]
    var newlineRequired = false

    for {
      infos <- trace.values
      info <- infos
    } {
      val strippedInfo = info.stripLineEnd
      val multiline = strippedInfo.contains('\n')

      if (lines.nonEmpty && (newlineRequired || multiline)) lines.append("")
      newlineRequired = multiline

      if (multiline) lines.append(strippedInfo) else lines.append(s"  $strippedInfo")
    }

    lines.reverseIterator.mkString("\n")
  }

  def format: String = {
    val lines = mutable.Buffer.empty[String]
    lines.append(s"${getClass.getName}: $getMessage")
    lines.append(formatTrace)

    var cause = getCause
    while (cause != null) {
      lines.append(s"Caused by ${cause.getClass.getName}: ${cause.getMessage}")
      for (elem <- cause.getStackTrace) lines.append(s"  $elem")
      cause = cause.getCause
    }

    lines.mkString("\n")
  }
}

object HippoException {
  private val walker = StackWalker.getInstance(StackWalker.Option.RETAIN_CLASS_REFERENCE)

  private val boringClasses = Seq[Class[_]](
    classOf[HippoException],
    classOf[HippoException.type],
    classOf[HippoNonfatalException.type],
    classOf[HippoContext],
    classOf[ProofChain],
  )

  private def isInterestingClass(clazz: Class[_]): Boolean = {
    for (boring <- boringClasses) if (boring.isAssignableFrom(clazz)) return false
    true
  }

  def wrap(e: Throwable): HippoException = e match {
    case e: HippoException => e
    case e => new HippoException("unexpected exception", cause = e)
  }

  @inline
  def fail(message: String, cause: Throwable = null): Nothing = throw new HippoException(message, cause)

  @inline
  def require(condition: Boolean, message: => String): Unit = if (condition) throw new HippoException(message)

  @inline
  def annotated[T](info: => String)(t: => T): T =
    try t
    catch { case e: HippoException => throw e.annotate(info) }
}

class HippoNonfatalException(message: String, cause: Throwable = null) extends HippoException(message, cause) {}

object HippoNonfatalException {
  def wrap(e: Throwable): HippoNonfatalException = e match {
    case e: HippoNonfatalException => e
    case e => new HippoNonfatalException("unexpected exception", cause = e)
  }

  @inline
  def fail(message: String, cause: Throwable = null): Nothing = throw new HippoNonfatalException(message, cause)

  @inline
  def require(condition: Boolean, message: => String): Unit = if (!condition) throw new HippoNonfatalException(message)
}
