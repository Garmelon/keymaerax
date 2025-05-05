/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.core
import org.keymaerax.core.{
  BaseVariable,
  DifferentialSymbol,
  DotFormula,
  DotTerm,
  Expression,
  Formula,
  FuncOf,
  NamedSymbol,
  Nothing,
  PredOf,
  PredicationalOf,
  Program,
  ProgramConst,
  Sequent,
  SystemConst,
  Term,
}
import org.keymaerax.hippocore.definitions.{Definitions, Name, Replacement}
import org.keymaerax.hippocore.proof.{HippoExpression, HippoSequent}
import org.keymaerax.hippocore.tools.ExprTransform
import org.keymaerax.infrastruct.Augmentors.ExpressionAugmentor

import scala.collection.{mutable, SortedMap}

/**
 * Insert dL expressions into other dL expressions, similar to string interpolation.
 *
 * A name without index that ends with two or more underscores is interpreted as a named placeholder. The interpolator
 * looks up the replacement expression using one of the provided lookup functions. The lookup name is the original name
 * with two trailing underscores removed (e.g. `foo__` -> `foo`, `bar____` -> `bar__`). Which lookup method the
 * interpreter uses depends on the context surrounding the placeholder. For example, a placeholder variable in a term
 * will use the term lookup function.
 *
 * Function replacements (names inside a [[FuncOf]], [[PredOf]], [[PredicateOf]]) are expected to use their arguments as
 * [[DotTerm]]s with the index denoting the position of the argument. For example, you would define an addition function
 * with two arguments as `._0 + ._1`. The number of arguments is determined by the placeholder context, not the
 * replacement's dot terms. There must not be more dot terms in the replacement expression than arguments in the
 * placeholder context. All dot terms must have an index.
 *
 * @param lookup
 *   Look up an [[Expression]] by placeholder name, throwing an exception if none exists.
 * @param onError
 *   Throw an appropriate exception. The first argument is the placeholder name at which interpolation failed. The
 *   second argument is a short description of the error.
 */
class Interpolator(val lookup: String => HippoExpression, val onError: (String, String) => Nothing) {
  private def lookupTerm(name: String): (Term, Definitions) = lookup(name) match {
    case HippoExpression(expr: Term, defs) => (expr, defs)
    case _ => onError(name, "replacement value must be a dL term")
  }

  private def lookupFormula(name: String): (Formula, Definitions) = lookup(name) match {
    case HippoExpression(expr: Formula, defs) => (expr, defs)
    case _ => onError(name, "replacement value must be a dL formula")
  }

  private def lookupProgram(name: String): (Program, Definitions) = lookup(name) match {
    case HippoExpression(expr: Program, defs) => (expr, defs)
    case _ => onError(name, "replacement value must be a dL program")
  }

  private def interpolatedName(symbol: NamedSymbol): Option[String] = {
    if (symbol.index.nonEmpty) return None
    // Maybe a name like "foo___" should be ignored, but for now it resolves to "foo_".
    if (!symbol.name.endsWith("__")) return None
    val name = symbol.name.stripSuffix("__")
    Some(name)
  }

  private class Transform extends ExprTransform {
    private val defs: mutable.Map[Name, Replacement] = mutable.Map.empty

    private def addDef(name: Name, repl: Replacement): Unit = {
      for (existingRepl <- defs.get(name)) require(repl == existingRepl)
      defs.put(name, repl)
    }

    private def addDefs(defs: Definitions): Unit = for ((name, repl) <- defs.byName) addDef(name, repl)

    def definitions: Definitions = Definitions(defs.to(SortedMap))

    override def ttBaseVariable(it: BaseVariable): Term = interpolatedName(it) match {
      case None => super.ttBaseVariable(it)
      case Some(name) => onError(name, "variables can't be placeholders, use zero-argument functions instead")
    }

    override def ttDifferentialSymbol(it: DifferentialSymbol): Term = interpolatedName(it) match {
      case None => super.ttDifferentialSymbol(it)
      case Some(name) => onError(name, "differential symbols are not valid placeholders")
    }

    override def ttFuncOf(it: FuncOf): Term = interpolatedName(it.func) match {
      case None => super.ttFuncOf(it)
      case Some(name) if it.func.interp.isDefined =>
        onError(name, "placeholder functions must not have an interpretation")
      case Some(name) =>
        val (term, defs) = lookupTerm(name)
        addDefs(defs)
        addDef(Name(it.func), Replacement.FuncOf(it.func.realDomainDim.get, term))
        it
    }

    override def tfPredOf(it: PredOf): Formula = interpolatedName(it.func) match {
      case None => super.tfPredOf(it)
      case Some(name) if it.func.interp.isDefined =>
        onError(name, "placeholder functions must not have an interpretation")
      case Some(name) =>
        val (fml, defs) = lookupFormula(name)
        addDefs(defs)
        addDef(Name(it.func), Replacement.PredOf(it.func.realDomainDim.get, fml))
        it
    }

    override def tfPredicationalOf(it: PredicationalOf): Formula = interpolatedName(it.func) match {
      case None => super.tfPredicationalOf(it)
      case Some(name) if it.func.interp.isDefined =>
        onError(name, "placeholder functions must not have an interpretation")
      case Some(name) =>
        val (fml, defs) = lookupFormula(name)
        addDefs(defs)
        addDef(Name(it.func), Replacement.PredicationalOf(fml))
        it
    }

    override def tpProgramConst(it: ProgramConst): Program = interpolatedName(it) match {
      case None => super.tpProgramConst(it)
      case Some(name) =>
        val (prog, defs) = lookupProgram(name)
        addDefs(defs)
        addDef(Name(it), Replacement.ProgramConst(prog, it.space))
        it
    }

    override def tpSystemConst(it: SystemConst): Program = interpolatedName(it) match {
      case None => super.tpSystemConst(it)
      case Some(name) =>
        val (prog, defs) = lookupProgram(name)
        addDefs(defs)
        addDef(Name(it), Replacement.SystemConst(prog, it.space))
        it
    }
  }

  /**
   * Interpolate an expression, similar to string interpolation. See [[Interpolator]] for more details.
   *
   * @param expression
   *   Expression to interpolate.
   * @return
   *   Interpolated expression.
   */
  def interpolate(expression: Expression): HippoExpression = {
    val tf = new Transform
    val newExpr = tf.transformExpression(expression)
    HippoExpression(newExpr, tf.definitions)
  }

  /**
   * Interpolate a sequent, similar to string interpolation. See [[Interpolator]] for more details.
   *
   * @param sequent
   *   Sequent to interpolate.
   * @return
   *   Interpolated sequent.
   */
  def interpolate(sequent: Sequent): HippoSequent = {
    val tf = new Transform
    val newSequent = Sequent(ante = sequent.ante.map(tf.transformFormula), succ = sequent.succ.map(tf.transformFormula))
    HippoSequent(newSequent, tf.definitions)
  }
}

object Interpolator {
  def replaceFuncArgs(args: Seq[String], term: Term): Term = {
    args
      .zipWithIndex
      .foldLeft(term) { case (term, (arg, i)) => term.replaceFree(BaseVariable(arg), DotTerm(idx = Some(i))) }
  }

  def replacePredArgs(args: Seq[String], formula: Formula): Formula = {
    args
      .zipWithIndex
      .foldLeft(formula) { case (formula, (arg, i)) => formula.replaceFree(BaseVariable(arg), DotTerm(idx = Some(i))) }
  }

  def replacePredicationalArg(arg: String, formula: Formula): Formula = {
    val function = core.Function(name = arg, domain = core.Unit, sort = core.Bool)
    formula.replaceAll(PredOf(func = function, child = Nothing), DotFormula)
  }
}
