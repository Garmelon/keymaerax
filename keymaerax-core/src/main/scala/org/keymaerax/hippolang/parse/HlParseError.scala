/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.parse

class HlParseError(slice: SourceFile#Slice, error: String, label: String) extends Exception(error) {
  def format: String = slice.formatError(error, label)
  def print(): Unit = println(format)
}
