/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.parse

import java.nio.file.Path

case class SourceFile(text: String, path: Option[Path] = None) {
  private val newlines = text.zipWithIndex.filter(_._1 == '\n').map(_._2)

  def lines: Int = newlines.length + 1

  def line(row: Int): Slice = {
    require(0 <= row && row <= newlines.length, "line out of range")
    val start = if (row > 0) newlines(row - 1) + 1 else 0
    val end = if (row < newlines.length) newlines(row) + 1 else text.length
    Slice(start, end)
  }

  def rowColAt(index: Int): (Int, Int) = {
    require(0 <= index && index <= text.length, "index out of range")
    val row = newlines.search(index).insertionPoint
    val col = index - line(row).start
    (row, col)
  }

  case class Slice(start: Int, end: Int) {
    require(start <= end, "invalid slice range")

    def text: String = SourceFile.this.text.slice(start, end)

    def format(label: String = ""): String = {
      val (startRow, startCol) = rowColAt(start)
      val (endRow, endCol) = rowColAt(end)
      val firstRow = (startRow - 1) max 0
      val lastRow = (endRow + 1) min (SourceFile.this.lines - 1)

      val lineNumberWidth = lastRow.toString.length
      val preEmpty = " " * lineNumberWidth
      def preOf(line: Int): String = line.toString.reverse.padTo(lineNumberWidth, ' ').reverse

      val lines = Seq.newBuilder[String]

      val location = s"[${startRow + 1}:${startCol + 1}]"
      path match {
        case Some(path) => lines.addOne(s" $preEmpty ,- $path $location")
        case None => lines.addOne(s" $preEmpty ,- $location")
      }

      lines.addOne(s" $preEmpty |")

      for (row <- firstRow to lastRow) {
        val rowText = line(row).text.stripLineEnd
        val lineNumber = preOf(row + 1)

        if ((row == firstRow || row == lastRow) && rowText.isBlank) () // Skip, otherwise it looks too spacious
        else if (startRow == endRow) lines.addOne(s" $lineNumber | $rowText")
        else if (row > startRow && row <= endRow) lines.addOne(s" $lineNumber | | $rowText")
        else lines.addOne(s" $lineNumber |   $rowText")

        val width = (endCol - startCol) max 1
        val labelText = if (label.isBlank) "" else s" $label"
        if (row == startRow && row == endRow) lines.addOne(s" $preEmpty | ${" " * startCol}${"^" * width}$labelText")
        else if (row == startRow) lines.addOne(s" $preEmpty | ,-${"-" * startCol}^")
        else if (row == endRow) lines.addOne(s" $preEmpty | '${"-" * endCol}^$labelText")
      }

      lines.addOne(s" $preEmpty-'")
      lines.result().mkString("\n")
    }
  }

  object Slice {
    def apply(at: Int): Slice = Slice(at, at)
  }
}
