/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.parse

import java.nio.file.Path

// TODO Remove or use
class SourceFile(val contents: String, val path: Option[Path] = None) {
  // TODO Convert index and ranges to row:column info (binary search on precomputed list?)
//  import scala.jdk.StreamConverters._
//  val newlines: IndexedSeq[Int] = contents.chars().toScala(IndexedSeq).zipWithIndex.filter(_._2 == '\n').map(_._1)
//  def rowAndColumn(index: Int): Option[(Int, Int)] = ???
}
