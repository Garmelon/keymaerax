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
  Pair,
  PredOf,
  PredicationalOf,
  Program,
  ProgramConst,
  StaticSemantics,
  SubstitutionPair,
  Term,
  USubst,
}
import org.keymaerax.hippolochos.tools.ExprTransform
import org.keymaerax.infrastruct.Augmentors.ExpressionAugmentor

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
class Interpolator(val lookup: String => Expression, val onError: (String, String) => Nothing) {
  private def lookupTerm(name: String): Term = lookup(name) match {
    case value: Term => value
    case _ => onError(name, "replacement value must be a dL term")
  }

  private def lookupFormula(name: String): Formula = lookup(name) match {
    case value: Formula => value
    case _ => onError(name, "replacement value must be a dL formula")
  }

  private def lookupProgram(name: String): Program = lookup(name) match {
    case value: Program => value
    case _ => onError(name, "replacement value must be a dL program")
  }

  private def interpolatedName(symbol: NamedSymbol): Option[String] = {
    if (symbol.index.nonEmpty) return None
    // Maybe a name like "foo___" should be ignored, but for now it resolves to "foo_".
    if (!symbol.name.endsWith("__")) return None
    val name = symbol.name.stripSuffix("__")
    Some(name)
  }

  private def argsFromPairs(term: Term): List[Term] = term match {
    case Nothing => Nil
    case term: Pair => term.left :: argsFromPairs(term.right)
    case term => term :: Nil
  }

  /** Find the indexes of all dot symbols used in an expression. */
  private def allDotIndexes(expression: Expression): Set[Option[Int]] = StaticSemantics
    .symbols(expression)
    .filter(_.isInstanceOf[DotTerm])
    .map(_.index)

  /**
   * Verify that the dot indexes are valid for this interpolation.
   *
   * There must be no dots without index, and all indexes must refer to existing arguments.
   */
  private def verifyDotsMatchArgs(name: String, dots: Set[Option[Int]], args: List[Term]): Unit = dots.foreach {
    case None => onError(name, "dots in function definition must have an index")
    case Some(index) if !args.indices.contains(index) => onError(name, "dot index out of range")
    case Some(_) =>
  }

  /**
   * Verify that the dot indexes are valid for this interpolation.
   *
   * There must only dots without indexes.
   */
  private def verifyDotMatchesArg(name: String, dots: Set[Option[Int]]): Unit = dots.foreach {
    case None =>
    case Some(_) => onError(name, "dots in function definition must have no index")
  }

  private def applyFunc(name: String, definition: Term, child: Term): Term = {
    val args = argsFromPairs(child)
    verifyDotsMatchArgs(name, allDotIndexes(definition), args)
    val substPairs = args.zipWithIndex.map { case (arg, i) => SubstitutionPair(DotTerm(idx = Some(i)), arg) }
    USubst(substPairs).apply(definition)
  }

  private def applyPred(name: String, definition: Formula, child: Term): Formula = {
    val args = argsFromPairs(child)
    verifyDotsMatchArgs(name, allDotIndexes(definition), args)
    val substPairs = args.zipWithIndex.map { case (arg, i) => SubstitutionPair(DotTerm(idx = Some(i)), arg) }
    USubst(substPairs).apply(definition)
  }

  private def applyPredicational(name: String, definition: Formula, child: Formula): Formula = {
    verifyDotMatchesArg(name, allDotIndexes(definition))
    val substPairs = Seq(SubstitutionPair(DotFormula, child))
    USubst(substPairs).apply(definition)
  }

  /**
   * Interpolate an expression, similar to string interpolation. See [[Interpolator]] for more details.
   *
   * @param expression
   *   Expression to interpolate.
   * @return
   *   Interpolated expression.
   */
  def interpolate(expression: Expression): Expression = new ExprTransform {
    override def ttBaseVariable(it: BaseVariable): Term = interpolatedName(it) match {
      case None => super.ttBaseVariable(it)
      case Some(name) => lookupTerm(name)
    }

    override def ttDifferentialSymbol(it: DifferentialSymbol): Term = interpolatedName(it) match {
      case None => super.ttDifferentialSymbol(it)
      case Some(name) => onError(name, "differential symbols are not valid placeholders")
    }

    override def ttFuncOf(it: FuncOf): Term = interpolatedName(it.func) match {
      case None => super.ttFuncOf(it)
      case Some(name) if it.func.interp.isDefined =>
        onError(name, "placeholder functions must not have an interpretation")
      case Some(name) => applyFunc(name, lookupTerm(name), transformTerm(it.child))
    }

    override def tfPredOf(it: PredOf): Formula = interpolatedName(it.func) match {
      case None => super.tfPredOf(it)
      case Some(name) if it.func.interp.isDefined =>
        onError(name, "placeholder functions must not have an interpretation")
      case Some(name) => applyPred(name, lookupFormula(name), transformTerm(it.child))
    }

    override def tfPredicationalOf(it: PredicationalOf): Formula = interpolatedName(it.func) match {
      case None => super.tfPredicationalOf(it)
      case Some(name) if it.func.interp.isDefined =>
        onError(name, "placeholder functions must not have an interpretation")
      case Some(name) => applyPredicational(name, lookupFormula(name), transformFormula(it.child))
    }

    override def tpProgramConst(it: ProgramConst): Program = interpolatedName(it) match {
      case None => super.tpProgramConst(it)
      case Some(name) => lookupProgram(name)
    }
  }.transformExpression(expression)
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
