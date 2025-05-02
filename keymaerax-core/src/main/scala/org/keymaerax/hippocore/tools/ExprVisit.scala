/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.tools

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
    case it: core.BaseVariable => vtBaseVariable(it)
    case it: core.Differential => vtDifferential(it)
    case it: core.DifferentialSymbol => vtDifferentialSymbol(it)
    case it: core.Divide => vtDivide(it)
    case it: core.DotTerm => vtDotTerm(it)
    case it: core.FuncOf => vtFuncOf(it)
    case it: core.Minus => vtMinus(it)
    case it: core.Neg => vtNeg(it)
    case core.Nothing => vtNothing
    case it: core.Number => vtNumber(it)
    case it: core.Pair => vtPair(it)
    case it: core.Plus => vtPlus(it)
    case it: core.Power => vtPower(it)
    case it: core.Times => vtTimes(it)
    case it: core.UnitFunctional => vtUnitFunctional(it)
  }

  final def visitFormula(it: core.Formula): T = it match {
    case it: core.And => vfAnd(it)
    case it: core.Box => vfBox(it)
    case it: core.Diamond => vfDiamond(it)
    case it: core.DifferentialFormula => vfDifferentialFormula(it)
    case core.DotFormula => vfDotFormula
    case it: core.Equal => vfEqual(it)
    case it: core.Equiv => vfEquiv(it)
    case it: core.Exists => vfExists(it)
    case core.False => vfFalse
    case it: core.Forall => vfForall(it)
    case it: core.Greater => vfGreater(it)
    case it: core.GreaterEqual => vfGreaterEqual(it)
    case it: core.Imply => vfImply(it)
    case it: core.Less => vfLess(it)
    case it: core.LessEqual => vfLessEqual(it)
    case it: core.Not => vfNot(it)
    case it: core.NotEqual => vfNotEqual(it)
    case it: core.Or => vfOr(it)
    case it: core.PredOf => vfPredOf(it)
    case it: core.PredicationalOf => vfPredicationalOf(it)
    case core.True => vfTrue
    case it: core.UnitPredicational => vfUnitPredicational(it)
  }

  final def visitProgram(it: core.Program): T = it match {
    case it: core.Assign => vpAssign(it)
    case it: core.AssignAny => vpAssignAny(it)
    case it: core.Choice => vpChoice(it)
    case it: core.Compose => vpCompose(it)
    case it: core.Dual => vpDual(it)
    case it: core.Loop => vpLoop(it)
    case it: core.ODESystem => vpODESystem(it)
    case it: core.ProgramConst => vpProgramConst(it)
    case it: core.SystemConst => vpSystemConst(it)
    case it: core.Test => vpTest(it)
    case _: core.DifferentialProgram => throw new IllegalArgumentException("malformed program")
  }

  final def visitDifferentialProgram(it: core.DifferentialProgram): T = it match {
    case it: core.AtomicODE => vdpAtomicODE(it)
    case it: core.DifferentialProduct => vdpDifferentialProduct(it)
    case it: core.DifferentialProgramConst => vdpDifferentialProgramConst(it)
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

  def vtBaseVariable(it: core.BaseVariable): T = default
  def vtDifferential(it: core.Differential): T = vt(it.child)
  def vtDifferentialSymbol(it: core.DifferentialSymbol): T = default
  def vtDivide(it: core.Divide): T = reduce(vt(it.left), vt(it.right))
  def vtDotTerm(it: core.DotTerm): T = default
  def vtFuncOf(it: core.FuncOf): T = vt(it.child)
  def vtMinus(it: core.Minus): T = reduce(vt(it.left), vt(it.right))
  def vtNeg(it: core.Neg): T = vt(it.child)
  def vtNothing: T = default
  def vtNumber(it: core.Number): T = default
  def vtPair(it: core.Pair): T = reduce(vt(it.left), vt(it.right))
  def vtPlus(it: core.Plus): T = reduce(vt(it.left), vt(it.right))
  def vtPower(it: core.Power): T = reduce(vt(it.left), vt(it.right))
  def vtTimes(it: core.Times): T = reduce(vt(it.left), vt(it.right))
  def vtUnitFunctional(it: core.UnitFunctional): T = default

  def vfAnd(it: core.And): T = reduce(vf(it.left), vf(it.right))
  def vfBox(it: core.Box): T = reduce(vp(it.program), vf(it.child))
  def vfDiamond(it: core.Diamond): T = reduce(vp(it.program), vf(it.child))
  def vfDifferentialFormula(it: core.DifferentialFormula): T = vf(it.child)
  def vfDotFormula: T = default
  def vfEqual(it: core.Equal): T = reduce(vt(it.left), vt(it.right))
  def vfEquiv(it: core.Equiv): T = reduce(vf(it.left), vf(it.right))
  def vfExists(it: core.Exists): T = vf(it.child)
  def vfFalse: T = default
  def vfForall(it: core.Forall): T = vf(it.child)
  def vfGreater(it: core.Greater): T = reduce(vt(it.left), vt(it.right))
  def vfGreaterEqual(it: core.GreaterEqual): T = reduce(vt(it.left), vt(it.right))
  def vfImply(it: core.Imply): T = reduce(vf(it.left), vf(it.right))
  def vfLess(it: core.Less): T = reduce(vt(it.left), vt(it.right))
  def vfLessEqual(it: core.LessEqual): T = reduce(vt(it.left), vt(it.right))
  def vfNot(it: core.Not): T = vf(it.child)
  def vfNotEqual(it: core.NotEqual): T = reduce(vt(it.left), vt(it.right))
  def vfOr(it: core.Or): T = reduce(vf(it.left), vf(it.right))
  def vfPredOf(it: core.PredOf): T = vt(it.child)
  def vfPredicationalOf(it: core.PredicationalOf): T = vf(it.child)
  def vfTrue: T = default
  def vfUnitPredicational(it: core.UnitPredicational): T = default

  def vpAssign(it: core.Assign): T = vt(it.e)
  def vpAssignAny(it: core.AssignAny): T = default
  def vpChoice(it: core.Choice): T = reduce(vp(it.left), vp(it.right))
  def vpCompose(it: core.Compose): T = reduce(vp(it.left), vp(it.right))
  def vpDual(it: core.Dual): T = vp(it.child)
  def vpLoop(it: core.Loop): T = vp(it.child)
  def vpODESystem(it: core.ODESystem): T = reduce(vdp(it.ode), vf(it.constraint))
  def vpProgramConst(it: core.ProgramConst): T = default
  def vpSystemConst(it: core.SystemConst): T = default
  def vpTest(it: core.Test): T = vf(it.cond)

  def vdpAtomicODE(it: core.AtomicODE): T = vt(it.e)
  def vdpDifferentialProduct(it: core.DifferentialProduct): T = reduce(vdp(it.left), vdp(it.right))
  def vdpDifferentialProgramConst(it: core.DifferentialProgramConst): T = default
}
