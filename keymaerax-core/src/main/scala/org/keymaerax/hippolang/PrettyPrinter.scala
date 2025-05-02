/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.core.{AntePos, Expression, Formula, Sequent, SuccPos}
import org.keymaerax.hippocore.definitions.{Definitions, Name, Replacement}
import org.keymaerax.hippocore.proof.{HippoExpression, HippoProof, HippoSequent}
import org.keymaerax.parser.KeYmaeraXPrettyPrinter

object PrettyPrinter {
  private val printer = KeYmaeraXPrettyPrinter

  def padLeft(width: Int)(str: String): String = " ".repeat(width - str.length) + str
  def indent(str: String, by: Int = 2): String = str.linesIterator.map(" ".repeat(by) + _).mkString("\n")

  private def printDefinition(name: Name, repl: Replacement): String = s"with ${repl.placeholder(name)} := ${repl.expr}"

  private def printDefinitions(defs: Definitions): String = defs
    .topologically
    .map(name => printDefinition(name, defs.byName(name)))
    .mkString("\n")

  private def printExpression(expr: Expression): String = printer(expr)

  def printHippoExpression(expr: HippoExpression): String = {
    val exprStr = printExpression(expr.expr)
    if (expr.defs.byName.isEmpty) return s"dL{ $exprStr }"
    val defsStr = printDefinitions(expr.defs)
    val inner = s"$defsStr\n$exprStr"
    s"dL{\n${indent(inner)}\n}"
  }

  private def printSequent(sequent: Sequent): String = {
    val antePositions = sequent.ante.indices.map(AntePos(_).getPos.toString)
    val succPositions = sequent.succ.indices.map(SuccPos(_).getPos.toString)
    val posWidth = 2 max (antePositions ++ succPositions).map(_.length).maxOption.getOrElse(0)
    val posPad = padLeft(posWidth)(_)

    def printSeqFormula(formula: Formula, pos: String): String =
      s"${posPad(pos)}: ${printExpression(formula)}    [${formula.getClass.getSimpleName}]"

    val ante = sequent.ante.zip(antePositions).map(it => printSeqFormula(it._1, it._2)).reverse
    val succ = sequent.succ.zip(succPositions).map(it => printSeqFormula(it._1, it._2))

    ((ante :+ "==>") ++ succ).mkString("\n")
  }

  def printHippoSequent(sequent: HippoSequent): String = {
    val sequentStr = printSequent(sequent.sequent)
    val defsStr = printDefinitions(sequent.defs)
    val inner = if (defsStr.isEmpty) sequentStr else s"$defsStr\n$sequentStr"
    s"dLs{\n${indent(inner)}\n}"
  }

  def printHippoProof(proof: HippoProof): String = {
    val premises = proof
      .premises
      .map {
        case p if p.mustBeProved => s"given! ${printHippoSequent(p.sequent)}"
        case p => s"given ${printHippoSequent(p.sequent)}"
      }
      .mkString("\n")

    val conclusion = s"proven ${printHippoSequent(proof.conclusion)}"

    if (premises.isEmpty) conclusion else s"$premises\n$conclusion"
  }
}
