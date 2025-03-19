/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.tools

import org.keymaerax.core

/** A visitor over expressions. */
trait ExprVisit[T] {
  def default: T
  def reduce(a: T, b: T): T

  final def visitExpression(it: core.Expression): T = it match {
    case it: core.Term => visitTerm(it)
    case it: core.Formula => visitFormula(it)
    case it: core.Program => visitProgram(it)
    case _ => throw new IllegalArgumentException("Unsupported expression")
  }

  final def visitTerm(it: core.Term): T = it match {
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

  final def visitFormula(it: core.Formula): T = it match {
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

  final def visitProgram(it: core.Program): T = it match {
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

  final def visitDifferentialProgram(it: core.DifferentialProgram): T = it match {
    case it: core.AtomicODE => tdpAtomicODE(it)
    case it: core.DifferentialProduct => tdpDifferentialProduct(it)
    case it: core.DifferentialProgramConst => tdpDifferentialProgramConst(it)
  }

  // Shortened method names for internal use

  @inline
  private final def vt(it: core.Term): T = visitTerm(it)
  @inline
  private final def vf(it: core.Formula): T = visitFormula(it)
  @inline
  private final def vp(it: core.Program): T = visitProgram(it)
  @inline
  private final def vdp(it: core.DifferentialProgram): T = visitDifferentialProgram(it)

  // Overridable methods

  def ttBaseVariable(it: core.BaseVariable): T = default
  def ttDifferential(it: core.Differential): T = vt(it.child)
  def ttDifferentialSymbol(it: core.DifferentialSymbol): T = default
  def ttDivide(it: core.Divide): T = reduce(vt(it.left), vt(it.right))
  def ttDotTerm(it: core.DotTerm): T = default
  def ttFuncOf(it: core.FuncOf): T = vt(it.child)
  def ttMinus(it: core.Minus): T = reduce(vt(it.left), vt(it.right))
  def ttNeg(it: core.Neg): T = vt(it.child)
  def ttNothing: T = default
  def ttNumber(it: core.Number): T = default
  def ttPair(it: core.Pair): T = reduce(vt(it.left), vt(it.right))
  def ttPlus(it: core.Plus): T = reduce(vt(it.left), vt(it.right))
  def ttPower(it: core.Power): T = reduce(vt(it.left), vt(it.right))
  def ttTimes(it: core.Times): T = reduce(vt(it.left), vt(it.right))
  def ttUnitFunctional(it: core.UnitFunctional): T = default

  def tfAnd(it: core.And): T = reduce(vf(it.left), vf(it.right))
  def tfBox(it: core.Box): T = reduce(vp(it.program), vf(it.child))
  def tfDiamond(it: core.Diamond): T = reduce(vp(it.program), vf(it.child))
  def tfDifferentialFormula(it: core.DifferentialFormula): T = vf(it.child)
  def tfDotFormula: T = default
  def tfEqual(it: core.Equal): T = reduce(vt(it.left), vt(it.right))
  def tfEquiv(it: core.Equiv): T = reduce(vf(it.left), vf(it.right))
  def tfExists(it: core.Exists): T = vf(it.child)
  def tfFalse: T = default
  def tfForall(it: core.Forall): T = vf(it.child)
  def tfGreater(it: core.Greater): T = reduce(vt(it.left), vt(it.right))
  def tfGreaterEqual(it: core.GreaterEqual): T = reduce(vt(it.left), vt(it.right))
  def tfImply(it: core.Imply): T = reduce(vf(it.left), vf(it.right))
  def tfLess(it: core.Less): T = reduce(vt(it.left), vt(it.right))
  def tfLessEqual(it: core.LessEqual): T = reduce(vt(it.left), vt(it.right))
  def tfNot(it: core.Not): T = vf(it.child)
  def tfNotEqual(it: core.NotEqual): T = reduce(vt(it.left), vt(it.right))
  def tfOr(it: core.Or): T = reduce(vf(it.left), vf(it.right))
  def tfPredOf(it: core.PredOf): T = vt(it.child)
  def tfPredicationalOf(it: core.PredicationalOf): T = vf(it.child)
  def tfTrue: T = default
  def tfUnitPredicational(it: core.UnitPredicational): T = default

  def tpAssign(it: core.Assign): T = vt(it.e)
  def tpAssignAny(it: core.AssignAny): T = default
  def tpChoice(it: core.Choice): T = reduce(vp(it.left), vp(it.right))
  def tpCompose(it: core.Compose): T = reduce(vp(it.left), vp(it.right))
  def tpDual(it: core.Dual): T = vp(it.child)
  def tpLoop(it: core.Loop): T = vp(it.child)
  def tpODESystem(it: core.ODESystem): T = reduce(vdp(it.ode), vf(it.constraint))
  def tpProgramConst(it: core.ProgramConst): T = default
  def tpSystemConst(it: core.SystemConst): T = default
  def tpTest(it: core.Test): T = vf(it.cond)

  def tdpAtomicODE(it: core.AtomicODE): T = vt(it.e)
  def tdpDifferentialProduct(it: core.DifferentialProduct): T = reduce(vdp(it.left), vdp(it.right))
  def tdpDifferentialProgramConst(it: core.DifferentialProgramConst): T = default
}
