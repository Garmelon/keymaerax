/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore

class HippoException(message: String, cause: Throwable = null) extends Exception(message, cause)

object HippoException {
  def apply(message: String, cause: Throwable = null): HippoException = new HippoException(message, cause)

  @inline
  def fail(message: String, cause: Throwable = null): Nothing = throw new HippoException(message, cause)

  @inline
  def require(condition: Boolean, message: => String): Unit = if (!condition) throw new HippoException(message)
}
