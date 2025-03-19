/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.tools

import org.keymaerax.core

/** A visitor that transforms expressions into other expressions. */
trait ExprTransform {
  final def transformExpression(it: core.Expression): core.Expression = it match {
    case it: core.Term => transformTerm(it)
    case it: core.Formula => transformFormula(it)
    case it: core.Program => transformProgram(it)
    case _ => throw new IllegalArgumentException("Unsupported expression")
  }

  final def transformTerm(it: core.Term): core.Term = it match {
    case it: core.BaseVariable => ttBaseVariable(it)
    case it: core.Differential => ttDifferential(it)
    case it: core.DifferentialSymbol => ttDifferentialSymbol(it)
    case it: core.Divide => ttDivide(it)
    case it: core.DotTerm => ttDotTerm(it)
    case it: core.FuncOf => ttFuncOf(it)
    case it: core.Minus => ttMinus(it)
    case it: core.Neg => ttNeg(it)
    case core.Nothing => ttNothing
    case it: core.Number => ttNumber(it)
    case it: core.Pair => ttPair(it)
    case it: core.Plus => ttPlus(it)
    case it: core.Power => ttPower(it)
    case it: core.Times => ttTimes(it)
    case it: core.UnitFunctional => ttUnitFunctional(it)
  }

  final def transformFormula(it: core.Formula): core.Formula = it match {
    case it: core.And => tfAnd(it)
    case it: core.Box => tfBox(it)
    case it: core.Diamond => tfDiamond(it)
    case it: core.DifferentialFormula => tfDifferentialFormula(it)
    case core.DotFormula => tfDotFormula
    case it: core.Equal => tfEqual(it)
    case it: core.Equiv => tfEquiv(it)
    case it: core.Exists => tfExists(it)
    case core.False => tfFalse
    case it: core.Forall => tfForall(it)
    case it: core.Greater => tfGreater(it)
    case it: core.GreaterEqual => tfGreaterEqual(it)
    case it: core.Imply => tfImply(it)
    case it: core.Less => tfLess(it)
    case it: core.LessEqual => tfLessEqual(it)
    case it: core.Not => tfNot(it)
    case it: core.NotEqual => tfNotEqual(it)
    case it: core.Or => tfOr(it)
    case it: core.PredOf => tfPredOf(it)
    case it: core.PredicationalOf => tfPredicationalOf(it)
    case core.True => tfTrue
    case it: core.UnitPredicational => tfUnitPredicational(it)
  }

  final def transformProgram(it: core.Program): core.Program = it match {
    case it: core.Assign => tpAssign(it)
    case it: core.AssignAny => tpAssignAny(it)
    case it: core.Choice => tpChoice(it)
    case it: core.Compose => tpCompose(it)
    case it: core.Dual => tpDual(it)
    case it: core.Loop => tpLoop(it)
    case it: core.ODESystem => tpODESystem(it)
    case it: core.ProgramConst => tpProgramConst(it)
    case it: core.SystemConst => tpSystemConst(it)
    case it: core.Test => tpTest(it)
    case _: core.DifferentialProgram => throw new IllegalArgumentException("malformed program")
  }

  final def transformDifferentialProgram(it: core.DifferentialProgram): core.DifferentialProgram = it match {
    case it: core.AtomicODE => tdpAtomicODE(it)
    case it: core.DifferentialProduct => tdpDifferentialProduct(it)
    case it: core.DifferentialProgramConst => tdpDifferentialProgramConst(it)
  }

  // Shortened method names for internal use

  @inline
  private final def tt(it: core.Term): core.Term = transformTerm(it)
  @inline
  private final def tf(it: core.Formula): core.Formula = transformFormula(it)
  @inline
  private final def tp(it: core.Program): core.Program = transformProgram(it)
  @inline
  private final def tdp(it: core.DifferentialProgram): core.DifferentialProgram = transformDifferentialProgram(it)

  // Overridable methods

  def ttBaseVariable(it: core.BaseVariable): core.Term = it
  def ttDifferential(it: core.Differential): core.Term = it.copy(child = tt(it.child))
  def ttDifferentialSymbol(it: core.DifferentialSymbol): core.Term = it
  def ttDivide(it: core.Divide): core.Term = it.copy(left = tt(it.left), right = tt(it.right))
  def ttDotTerm(it: core.DotTerm): core.Term = it
  def ttFuncOf(it: core.FuncOf): core.Term = it.copy(child = tt(it.child))
  def ttMinus(it: core.Minus): core.Term = it.copy(left = tt(it.left), right = tt(it.right))
  def ttNeg(it: core.Neg): core.Term = it.copy(child = tt(it.child))
  def ttNothing: core.Term = core.Nothing
  def ttNumber(it: core.Number): core.Term = it
  def ttPair(it: core.Pair): core.Term = it.copy(left = tt(it.left), right = tt(it.right))
  def ttPlus(it: core.Plus): core.Term = it.copy(left = tt(it.left), right = tt(it.right))
  def ttPower(it: core.Power): core.Term = it.copy(left = tt(it.left), right = tt(it.right))
  def ttTimes(it: core.Times): core.Term = it.copy(left = tt(it.left), right = tt(it.right))
  def ttUnitFunctional(it: core.UnitFunctional): core.Term = it

  def tfAnd(it: core.And): core.Formula = it.copy(left = tf(it.left), right = tf(it.right))
  def tfBox(it: core.Box): core.Formula = it.copy(program = tp(it.program), child = tf(it.child))
  def tfDiamond(it: core.Diamond): core.Formula = it.copy(program = tp(it.program), child = tf(it.child))
  def tfDifferentialFormula(it: core.DifferentialFormula): core.Formula = it.copy(child = tf(it.child))
  def tfDotFormula: core.Formula = core.DotFormula
  def tfEqual(it: core.Equal): core.Formula = it.copy(left = tt(it.left), right = tt(it.right))
  def tfEquiv(it: core.Equiv): core.Formula = it.copy(left = tf(it.left), right = tf(it.right))
  def tfExists(it: core.Exists): core.Formula = it.copy(child = tf(it.child))
  def tfFalse: core.Formula = core.False
  def tfForall(it: core.Forall): core.Formula = it.copy(child = tf(it.child))
  def tfGreater(it: core.Greater): core.Formula = it.copy(left = tt(it.left), right = tt(it.right))
  def tfGreaterEqual(it: core.GreaterEqual): core.Formula = it.copy(left = tt(it.left), right = tt(it.right))
  def tfImply(it: core.Imply): core.Formula = it.copy(left = tf(it.left), right = tf(it.right))
  def tfLess(it: core.Less): core.Formula = it.copy(left = tt(it.left), right = tt(it.right))
  def tfLessEqual(it: core.LessEqual): core.Formula = it.copy(left = tt(it.left), right = tt(it.right))
  def tfNot(it: core.Not): core.Formula = it.copy(child = tf(it.child))
  def tfNotEqual(it: core.NotEqual): core.Formula = it.copy(left = tt(it.left), right = tt(it.right))
  def tfOr(it: core.Or): core.Formula = it.copy(left = tf(it.left), right = tf(it.right))
  def tfPredOf(it: core.PredOf): core.Formula = it.copy(child = tt(it.child))
  def tfPredicationalOf(it: core.PredicationalOf): core.Formula = it.copy(child = tf(it.child))
  def tfTrue: core.Formula = core.True
  def tfUnitPredicational(it: core.UnitPredicational): core.Formula = it

  def tpAssign(it: core.Assign): core.Program = it.copy(e = tt(it.e))
  def tpAssignAny(it: core.AssignAny): core.Program = it
  def tpChoice(it: core.Choice): core.Program = it.copy(left = tp(it.left), right = tp(it.right))
  def tpCompose(it: core.Compose): core.Program = it.copy(left = tp(it.left), right = tp(it.right))
  def tpDual(it: core.Dual): core.Program = it.copy(child = tp(it.child))
  def tpLoop(it: core.Loop): core.Program = it.copy(child = tp(it.child))
  def tpODESystem(it: core.ODESystem): core.Program = it.copy(ode = tdp(it.ode), constraint = tf(it.constraint))
  def tpProgramConst(it: core.ProgramConst): core.Program = it
  def tpSystemConst(it: core.SystemConst): core.Program = it
  def tpTest(it: core.Test): core.Program = it.copy(cond = tf(it.cond))

  def tdpAtomicODE(it: core.AtomicODE): core.DifferentialProgram = it.copy(e = tt(it.e))
  def tdpDifferentialProduct(it: core.DifferentialProduct): core.DifferentialProgram = core
    .DifferentialProduct(left = tdp(it.left), right = tdp(it.right))
  def tdpDifferentialProgramConst(it: core.DifferentialProgramConst): core.DifferentialProgram = it
}
