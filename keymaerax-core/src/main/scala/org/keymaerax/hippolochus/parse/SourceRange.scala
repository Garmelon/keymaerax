/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.parse

// TODO Remove or use
case class SourceRange(source: SourceFile, startIndex: Int, endIndex: Int) {
  def merge(other: SourceRange): SourceRange = {
    require(source == other.source)
    copy(startIndex = startIndex.min(other.startIndex), endIndex = endIndex.min(other.endIndex))
  }

  override def toString: String = s"[$startIndex:$endIndex]"
}
