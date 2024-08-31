/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.tools

import org.keymaerax.core

import scala.annotation.tailrec

case class ExprPath(segments: List[Int]) {
  def select(expr: core.Term): core.Expression = ExprPath.selectAt(segments, expr)

  def select(expr: core.Formula): core.Expression = ExprPath.selectAt(segments, expr)

  def select(expr: core.Program): core.Expression = ExprPath.selectAt(segments, expr)

  def select(expr: core.Expression): core.Expression = ExprPath.selectAt(segments, expr)

  def replace(expr: core.Term, replacement: core.Expression): core.Term = ExprPath
    .replaceAt(segments, expr, replacement)

  def replace(expr: core.Formula, replacement: core.Expression): core.Formula = ExprPath
    .replaceAt(segments, expr, replacement)

  def replace(expr: core.Program, replacement: core.Expression): core.Program = ExprPath
    .replaceAt(segments, expr, replacement)

  def replace(expr: core.Expression, replacement: core.Expression): core.Expression = ExprPath
    .replaceAt(segments, expr, replacement)
}

object ExprPath {
  def apply(segments: Int*): ExprPath = ExprPath(segments.toList)

  private def invalidPath(index: Int, expr: core.Expression): Nothing =
    throw new IllegalArgumentException(s"invalid path, ${expr.getClass.getSimpleName} has no child $index")

  private def invalidReplacement(expected: String, got: core.Expression): Nothing =
    throw new IllegalArgumentException(s"invalid replacement, expected $expected but got ${got.getClass.getSimpleName}")

  private def invalidExpression(): Nothing =
    throw new IllegalArgumentException(s"invalid expression, expected term, formula, or program")

  @tailrec
  def selectAt(path: List[Int], expr: core.Term): core.Expression = path match {
    case Nil => expr
    case head :: next => (head, expr) match {
        // One child
        case (0, expr: core.FuncOf) => selectAt(next, expr.child)
        case (0, expr: core.Neg) => selectAt(next, expr.child)
        case (0, expr: core.Differential) => selectAt(next, expr.child)
        // Two children
        case (0, expr: core.Plus) => selectAt(next, expr.left)
        case (1, expr: core.Plus) => selectAt(next, expr.right)
        case (0, expr: core.Minus) => selectAt(next, expr.left)
        case (1, expr: core.Minus) => selectAt(next, expr.right)
        case (0, expr: core.Times) => selectAt(next, expr.left)
        case (1, expr: core.Times) => selectAt(next, expr.right)
        case (0, expr: core.Divide) => selectAt(next, expr.left)
        case (1, expr: core.Divide) => selectAt(next, expr.right)
        case (0, expr: core.Power) => selectAt(next, expr.left)
        case (1, expr: core.Power) => selectAt(next, expr.right)
        case (0, expr: core.Pair) => selectAt(next, expr.left)
        case (1, expr: core.Pair) => selectAt(next, expr.right)
        // Otherwise...
        case _ => invalidPath(head, expr)
      }
  }

  @tailrec
  def selectAt(path: List[Int], expr: core.Formula): core.Expression = path match {
    case Nil => expr
    case head :: next => (head, expr) match {
        // One child
        case (0, expr: core.PredOf) => selectAt(next, expr.child)
        case (0, expr: core.PredicationalOf) => selectAt(next, expr.child)
        case (0, expr: core.Not) => selectAt(next, expr.child)
        case (0, expr: core.Forall) => selectAt(next, expr.child)
        case (0, expr: core.Exists) => selectAt(next, expr.child)
        case (0, expr: core.DifferentialFormula) => selectAt(next, expr.child)
        // Two children
        case (0, expr: core.Equal) => selectAt(next, expr.left)
        case (1, expr: core.Equal) => selectAt(next, expr.right)
        case (0, expr: core.NotEqual) => selectAt(next, expr.left)
        case (1, expr: core.NotEqual) => selectAt(next, expr.right)
        case (0, expr: core.GreaterEqual) => selectAt(next, expr.left)
        case (1, expr: core.GreaterEqual) => selectAt(next, expr.right)
        case (0, expr: core.Greater) => selectAt(next, expr.left)
        case (1, expr: core.Greater) => selectAt(next, expr.right)
        case (0, expr: core.LessEqual) => selectAt(next, expr.left)
        case (1, expr: core.LessEqual) => selectAt(next, expr.right)
        case (0, expr: core.Less) => selectAt(next, expr.left)
        case (1, expr: core.Less) => selectAt(next, expr.right)
        case (0, expr: core.And) => selectAt(next, expr.left)
        case (1, expr: core.And) => selectAt(next, expr.right)
        case (0, expr: core.Or) => selectAt(next, expr.left)
        case (1, expr: core.Or) => selectAt(next, expr.right)
        case (0, expr: core.Imply) => selectAt(next, expr.left)
        case (1, expr: core.Imply) => selectAt(next, expr.right)
        case (0, expr: core.Equiv) => selectAt(next, expr.left)
        case (1, expr: core.Equiv) => selectAt(next, expr.right)
        case (0, expr: core.Box) => selectAt(next, expr.program)
        case (1, expr: core.Box) => selectAt(next, expr.child)
        case (0, expr: core.Diamond) => selectAt(next, expr.program)
        case (1, expr: core.Diamond) => selectAt(next, expr.child)
        // Otherwise...
        case _ => invalidPath(head, expr)
      }
  }

  @tailrec
  def selectAt(path: List[Int], expr: core.Program): core.Expression = path match {
    case Nil => expr
    case head :: next => (head, expr) match {
        // One child
        case (0, expr: core.Assign) => selectAt(next, expr.e)
        case (0, expr: core.Test) => selectAt(next, expr.cond)
        case (0, expr: core.Loop) => selectAt(next, expr.child)
        case (0, expr: core.Dual) => selectAt(next, expr.child)
        // Two children
        case (0, expr: core.Choice) => selectAt(next, expr.left)
        case (1, expr: core.Choice) => selectAt(next, expr.right)
        case (0, expr: core.Compose) => selectAt(next, expr.left)
        case (1, expr: core.Compose) => selectAt(next, expr.right)
        case (0, expr: core.ODESystem) => selectAt(next, expr.ode)
        case (1, expr: core.ODESystem) => selectAt(next, expr.constraint)
        // Otherwise...
        case _ => invalidPath(head, expr)
      }
  }

  @tailrec
  def selectAt(path: List[Int], expr: core.DifferentialProgram): core.Expression = path match {
    case Nil => expr
    case head :: next => (head, expr) match {
        // One child
        case (0, expr: core.AtomicODE) => selectAt(next, expr.e)
        // Two children
        case (0, expr: core.DifferentialProduct) => selectAt(next, expr.left)
        case (1, expr: core.DifferentialProduct) => selectAt(next, expr.right)
        // Otherwise...
        case _ => invalidPath(head, expr)
      }
  }

  def selectAt(path: List[Int], expr: core.Expression): core.Expression = expr match {
    case expr: core.Term => selectAt(path, expr)
    case expr: core.Formula => selectAt(path, expr)
    case expr: core.Program => selectAt(path, expr)
    case _ => invalidExpression()
  }

  def replaceAt(path: List[Int], expr: core.Term, replacement: core.Expression): core.Term = path match {
    case Nil => replacement match {
        case expr: core.Term => expr
        case _ => invalidReplacement(expected = "term", got = replacement)
      }
    case head :: next => (head, expr) match {
        // One child
        case (0, expr: core.FuncOf) => expr.copy(child = replaceAt(next, expr.child, replacement))
        case (0, expr: core.Neg) => expr.copy(child = replaceAt(next, expr.child, replacement))
        case (0, expr: core.Differential) => expr.copy(child = replaceAt(next, expr.child, replacement))
        // Two children
        case (0, expr: core.Plus) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Plus) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.Minus) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Minus) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.Times) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Times) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.Divide) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Divide) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.Power) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Power) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.Pair) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Pair) => expr.copy(right = replaceAt(next, expr.right, replacement))
        // Otherwise...
        case _ => invalidPath(head, expr)
      }
  }

  def replaceAt(path: List[Int], expr: core.Formula, replacement: core.Expression): core.Formula = path match {
    case Nil => replacement match {
        case expr: core.Formula => expr
        case _ => invalidReplacement(expected = "formula", got = replacement)
      }
    case head :: next => (head, expr) match {
        // One child
        case (0, expr: core.PredOf) => expr.copy(child = replaceAt(next, expr.child, replacement))
        case (0, expr: core.PredicationalOf) => expr.copy(child = replaceAt(next, expr.child, replacement))
        case (0, expr: core.Not) => expr.copy(child = replaceAt(next, expr.child, replacement))
        case (0, expr: core.Forall) => expr.copy(child = replaceAt(next, expr.child, replacement))
        case (0, expr: core.Exists) => expr.copy(child = replaceAt(next, expr.child, replacement))
        case (0, expr: core.DifferentialFormula) => expr.copy(child = replaceAt(next, expr.child, replacement))
        // Two children
        case (0, expr: core.Equal) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Equal) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.NotEqual) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.NotEqual) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.GreaterEqual) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.GreaterEqual) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.Greater) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Greater) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.LessEqual) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.LessEqual) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.Less) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Less) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.And) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.And) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.Or) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Or) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.Imply) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Imply) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.Equiv) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Equiv) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.Box) => expr.copy(program = replaceAt(next, expr.program, replacement))
        case (1, expr: core.Box) => expr.copy(child = replaceAt(next, expr.child, replacement))
        case (0, expr: core.Diamond) => expr.copy(program = replaceAt(next, expr.program, replacement))
        case (1, expr: core.Diamond) => expr.copy(child = replaceAt(next, expr.child, replacement))
        // Otherwise...
        case _ => invalidPath(head, expr)
      }
  }

  def replaceAt(path: List[Int], expr: core.Program, replacement: core.Expression): core.Program = path match {
    case Nil => replacement match {
        case expr: core.Program => expr
        case _ => invalidReplacement(expected = "program", got = replacement)
      }
    case head :: next => (head, expr) match {
        // One child
        case (0, expr: core.Assign) => expr.copy(e = replaceAt(next, expr.e, replacement))
        case (0, expr: core.Test) => expr.copy(cond = replaceAt(next, expr.cond, replacement))
        case (0, expr: core.Loop) => expr.copy(child = replaceAt(next, expr.child, replacement))
        case (0, expr: core.Dual) => expr.copy(child = replaceAt(next, expr.child, replacement))
        // Two children
        case (0, expr: core.Choice) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Choice) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.Compose) => expr.copy(left = replaceAt(next, expr.left, replacement))
        case (1, expr: core.Compose) => expr.copy(right = replaceAt(next, expr.right, replacement))
        case (0, expr: core.ODESystem) => expr.copy(ode = replaceAt(next, expr.ode, replacement))
        case (1, expr: core.ODESystem) => expr.copy(constraint = replaceAt(next, expr.constraint, replacement))
        // Otherwise...
        case _ => invalidPath(head, expr)
      }
  }

  def replaceAt(
      path: List[Int],
      expr: core.DifferentialProgram,
      replacement: core.Expression,
  ): core.DifferentialProgram = path match {
    case Nil => replacement match {
        case expr: core.DifferentialProgram => expr
        case _ => invalidReplacement(expected = "differential program", got = replacement)
      }
    case head :: next => (head, expr) match {
        // One child
        case (0, expr: core.AtomicODE) => expr.copy(e = replaceAt(next, expr.e, replacement))
        // Two children
        case (0, expr: core.DifferentialProduct) =>
          core.DifferentialProduct(replaceAt(next, expr.left, replacement), expr.right)
        case (1, expr: core.DifferentialProduct) =>
          core.DifferentialProduct(expr.left, replaceAt(next, expr.right, replacement))
        // Otherwise...
        case _ => invalidPath(head, expr)
      }
  }

  def replaceAt(path: List[Int], expr: core.Expression, replacement: core.Expression): core.Expression = expr match {
    case expr: core.Term => replaceAt(path, expr, replacement)
    case expr: core.Formula => replaceAt(path, expr, replacement)
    case expr: core.Program => replaceAt(path, expr, replacement)
    case _ => invalidExpression()
  }
}
