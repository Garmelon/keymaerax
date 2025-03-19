/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.tools

import org.keymaerax.core.Sequent
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
}
