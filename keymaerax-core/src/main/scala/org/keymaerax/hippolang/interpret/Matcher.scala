/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.core.*
import org.keymaerax.hippocore.proof.{HippoExpression, HippoSequent}
import org.keymaerax.hippolang.{HlangException, HlangValue}

import scala.collection.mutable

private class Matcher {
  val matches: mutable.Map[String, Expression] = mutable.Map.empty

  def addMatch(name: String, value: Expression): Unit = {
    val replaced = matches.put(name, value)
    if (replaced.isDefined) throw HlangException(s"Name $name occurs more than once in match pattern")
  }

  def addMatch(symbol: NamedSymbol, value: Expression): Unit = {
    val name = Interpolator.interpolatedName(symbol).get
    addMatch(name, value)
  }

  def isPlaceholder(symbol: NamedSymbol): Boolean = Interpolator.interpolatedName(symbol).isDefined

  final def matchSequent(pattern: Sequent, value: Sequent): Boolean = {
    if (pattern.ante.length != value.ante.length) return false
    if (pattern.succ.length != value.succ.length) return false
    for ((pa, va) <- pattern.ante.zip(value.ante)) if (!matchExpression(pa, va)) return false
    for ((ps, vs) <- pattern.succ.zip(value.succ)) if (!matchExpression(ps, vs)) return false
    true
  }

  final def matchExpression(pattern: Expression, value: Expression): Boolean = (pattern, value) match {
    case (p: Term, v: Term) => matchTerm(p, v)
    case (p: Formula, v: Formula) => matchFormula(p, v)
    case (p: Program, v: Program) => matchProgram(p, v)
    case _ => throw new IllegalArgumentException("Unsupported expression")
  }

  final def matchTerm(pattern: Term, value: Term): Boolean = (pattern, value) match {
    // Placeholder matches
    case (p: BaseVariable, v) if isPlaceholder(p) => addMatch(p, v); true
    // Structural matches
    case (p: BaseVariable, v: BaseVariable) => p == v
    case (p: Differential, v: Differential) => matchTerm(p.child, v.child)
    case (p: DifferentialSymbol, v: DifferentialSymbol) => p == v
    case (p: Divide, v: Divide) => matchTerm(p.left, v.left) && matchTerm(p.right, v.right)
    case (p: DotTerm, v: DotTerm) => p == v
    case (p: FuncOf, v: FuncOf) => p.func == v.func && matchTerm(p.child, v.child)
    case (p: Minus, v: Minus) => matchTerm(p.left, v.left) && matchTerm(p.right, v.right)
    case (p: Neg, v: Neg) => matchTerm(p.child, v.child)
    case (Nothing, Nothing) => true
    case (p: Number, v: Number) => p == v
    case (p: Pair, v: Pair) => matchTerm(p.left, v.left) && matchTerm(p.right, v.right)
    case (p: Plus, v: Plus) => matchTerm(p.left, v.left) && matchTerm(p.right, v.right)
    case (p: Power, v: Power) => matchTerm(p.left, v.left) && matchTerm(p.right, v.right)
    case (p: Times, v: Times) => matchTerm(p.left, v.left) && matchTerm(p.right, v.right)
    case (p: UnitFunctional, v: UnitFunctional) => p == v
    case _ => false
  }

  final def matchFormula(pattern: Formula, value: Formula): Boolean = (pattern, value) match {
    // Placeholder matches
    case (p: PredOf, v)
        if isPlaceholder(p.func) && !p.func.interpreted && p.func.domain == Unit && p.child == Nothing =>
      addMatch(p.func, v); true
    // Structural matches
    case (p: And, v: And) => matchFormula(p.left, v.left) && matchFormula(p.right, v.right)
    case (p: Box, v: Box) => matchProgram(p.program, v.program) && matchFormula(p.child, v.child)
    case (p: Diamond, v: Diamond) => matchProgram(p.program, v.program) && matchFormula(p.child, v.child)
    case (p: DifferentialFormula, v: DifferentialFormula) => matchFormula(p.child, v.child)
    case (DotFormula, DotFormula) => true
    case (p: Equal, v: Equal) => matchTerm(p.left, v.left) && matchTerm(p.right, v.right)
    case (p: Equiv, v: Equiv) => matchFormula(p.left, v.left) && matchFormula(p.right, v.right)
    case (p: Exists, v: Exists) => p.vars == v.vars && matchFormula(p.child, v.child)
    case (False, False) => true
    case (p: Forall, v: Forall) => p.vars == v.vars && matchFormula(p.child, v.child)
    case (p: Greater, v: Greater) => matchTerm(p.left, v.left) && matchTerm(p.right, v.right)
    case (p: GreaterEqual, v: GreaterEqual) => matchTerm(p.left, v.left) && matchTerm(p.right, v.right)
    case (p: Imply, v: Imply) => matchFormula(p.left, v.left) && matchFormula(p.right, v.right)
    case (p: Less, v: Less) => matchTerm(p.left, v.left) && matchTerm(p.right, v.right)
    case (p: LessEqual, v: LessEqual) => matchTerm(p.left, v.left) && matchTerm(p.right, v.right)
    case (p: Not, v: Not) => matchFormula(p.child, v.child)
    case (p: NotEqual, v: NotEqual) => matchTerm(p.left, v.left) && matchTerm(p.right, v.right)
    case (p: Or, v: Or) => matchFormula(p.left, v.left) && matchFormula(p.right, v.right)
    case (p: PredOf, v: PredOf) => p.func == v.func && matchTerm(p.child, v.child)
    case (p: PredicationalOf, v: PredicationalOf) => p.func == v.func && matchFormula(p.child, v.child)
    case (True, True) => true
    case (p: UnitPredicational, v: UnitPredicational) => p == v
    case _ => false
  }

  final def matchProgram(pattern: Program, value: Program): Boolean = (pattern, value) match {
    // Placeholder matches
    case (p: ProgramConst, v) if isPlaceholder(p) && p.space == AnyArg => addMatch(p, v); true
    // Structural matches
    case (p: Assign, v: Assign) => p.x == v.x && matchTerm(p.e, v.e)
    case (p: AssignAny, v: AssignAny) => p.x == v.x
    case (p: Choice, v: Choice) => matchProgram(p.left, v.left) && matchProgram(p.right, v.right)
    case (p: Compose, v: Compose) => matchProgram(p.left, v.left) && matchProgram(p.right, v.right)
    case (p: Dual, v: Dual) => matchProgram(p.child, v.child)
    case (p: Loop, v: Loop) => matchProgram(p.child, v.child)
    case (p: ODESystem, v: ODESystem) => matchDifferentialProgram(p.ode, v.ode) &&
      matchFormula(p.constraint, v.constraint)
    case (p: ProgramConst, v: ProgramConst) => p == v
    case (p: SystemConst, v: SystemConst) => p == v
    case (p: Test, v: Test) => matchFormula(p.cond, v.cond)
    case (_: DifferentialProgram, _) => throw new IllegalArgumentException("malformed program")
    case (_, _: DifferentialProgram) => throw new IllegalArgumentException("malformed program")
    case _ => false
  }

  final def matchDifferentialProgram(pattern: DifferentialProgram, value: DifferentialProgram): Boolean =
    (pattern, value) match {
      // Structural matches
      case (p: AtomicODE, v: AtomicODE) => matchTerm(p.xp, v.xp) && matchTerm(p.e, v.e)
      case (p: DifferentialProduct, v: DifferentialProduct) => matchDifferentialProgram(p.left, v.left) &&
        matchDifferentialProgram(p.right, v.right)
      case (p: DifferentialProgramConst, v: DifferentialProgramConst) => p == v
      case _ => false
    }
}

object Matcher {
  def matchExpression(pattern: Expression, value: Expression): Option[Map[String, Expression]] = {
    val matcher = new Matcher()
    if (matcher.matchExpression(pattern, value)) Some(matcher.matches.toMap) else None
  }

  def matchSequent(pattern: Sequent, value: Sequent): Option[Map[String, Expression]] = {
    val matcher = new Matcher()
    if (matcher.matchSequent(pattern, value)) Some(matcher.matches.toMap) else None
  }

  def matchHippoExpression(pattern: HippoExpression, value: HippoExpression): Option[Map[String, HippoExpression]] =
    matchExpression(pattern.expr, value.expr).map(_.view.mapValues(HippoExpression(_, defs = value.defs)).toMap)

  def matchHippoSequent(pattern: HippoSequent, value: HippoSequent): Option[Map[String, HippoExpression]] =
    matchSequent(pattern.sequent, value.sequent).map(_.view.mapValues(HippoExpression(_, defs = value.defs)).toMap)

  def matchValue(pattern: HlangValue, value: HlangValue): Option[Map[String, HlangValue]] = (pattern, value) match {
    case (p: HlangValue.DlExpression, v: HlangValue.DlExpression) =>
      matchHippoExpression(p.value, v.value).map(_.view.mapValues(HlangValue.DlExpression(_)).toMap)

    case (p: HlangValue.DlSequent, v: HlangValue.DlSequent) =>
      matchHippoSequent(p.value, v.value).map(_.view.mapValues(HlangValue.DlExpression(_)).toMap)

    case _ => None
  }
}
