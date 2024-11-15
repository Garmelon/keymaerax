/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos

class HippoException(message: String, cause: Throwable = null) extends Exception(message, cause)

object HippoException {
  def apply(message: String, cause: Throwable = null): HippoException = new HippoException(message, cause)
}
