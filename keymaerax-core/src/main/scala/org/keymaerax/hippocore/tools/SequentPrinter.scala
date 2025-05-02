/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.tools

import org.keymaerax.core.{AntePos, Sequent, SuccPos}
import org.keymaerax.hippocore.proof.HippoSequent
import org.keymaerax.parser.KeYmaeraXPrettyPrinter

object SequentPrinter {
  private val printer = KeYmaeraXPrettyPrinter

  /**
   * Format a [[Sequent]] in a single line.
   *
   * Compatible with [[org.keymaerax.parser.DLParser.sequent]].
   */
  def oneline(sequent: Sequent): String = {
    val antes = sequent.ante.map(printer).mkString(",  ")
    val succs = sequent.succ.map(printer).mkString(",  ")
    s"$antes  ==>  $succs".trim
  }

  /**
   * Format a [[Sequent]] in a multi-line format with at most one antecedent or succedent per line.
   *
   * Compatible with [[org.keymaerax.parser.DLParser.sequent]].
   */
  def multiline(sequent: Sequent): String = {
    val antes = sequent.ante.map(printer).mkString(",\n")
    val succs = sequent.succ.map(printer).mkString(",\n")
    s"$antes\n==>\n$succs".trim
  }

  /** Automatically chooses between [[oneline]] and [[multiline]] depending on the size of the [[Sequent]]. */
  def smart(sequent: Sequent): String =
    if (sequent.ante.length <= 1 && sequent.succ.length <= 1) oneline(sequent) else multiline(sequent)

  def multiline(sequent: HippoSequent): String = {
    val lines = Seq.newBuilder[String]

    val anteIndices = sequent.sequent.ante.indices.map(AntePos(_).getIndex)
    val succIndices = sequent.sequent.succ.indices.map(SuccPos(_).getIndex)
    val indent = (anteIndices ++ succIndices).map(_.toString.length).maxOption.getOrElse(0) max 2
    def leftPad(s: String) = s.reverse.padTo(indent, ' ').reverse

    for (name <- sequent.defs.topologically) lines.addOne(s"with $name := ${sequent.defs.byName(name)}")
    for ((fml, pos) <- sequent.sequent.ante.zip(anteIndices).reverse) lines
      .addOne(s"${leftPad(pos.toString)}: $fml    [${fml.getClass.getName}]")
    lines.addOne("==>")
    for ((fml, pos) <- sequent.sequent.succ.zip(succIndices)) lines
      .addOne(s"${leftPad(pos.toString)}: $fml    [${fml.getClass.getName}]")

    lines.result().mkString("\n")
  }
}
