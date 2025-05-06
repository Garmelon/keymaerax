/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.definitions

import org.keymaerax.core.{Expression, Provable, Sequent, StaticSemantics, SubstitutionPair, URename, USubst}
import org.keymaerax.hippocore.tools.{Hashable, Hasher}

import scala.collection.{mutable, MapView, SortedMap, SortedSet}

/**
 * A set of named definitions.
 *
 * @param byName
 *   Definitions, by name.
 */
case class Definitions(byName: SortedMap[Name, Replacement]) extends Hashable {
  def names: SortedSet[Name] = byName.keySet

  /**
   * Subdefinitions of every definition, by name.
   *
   * @see
   *   [[subdefinitions]]
   */
  val subdefsByName: MapView[Name, Set[Name]] = byName.view.mapValues(repl => subdefinitions(repl.expr))

  /**
   * Definitions sorted topologically so that a definition always comes before its uses.
   *
   * @see
   *   [[https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search]]
   */
  val topologically: Seq[Name] = {
    val visiting = mutable.Set.empty[Name]
    val visited = mutable.Set.empty[Name]
    val result = Seq.newBuilder[Name]

    def visit(node: Name): Unit = {
      if (visited.contains(node)) return
      require(!visiting.contains(node), "definition cycle detected")
      visiting.add(node)
      for (child <- subdefsByName(node).toSeq.sorted) visit(child)
      visited.add(node)
      result.addOne(node)
    }

    for (node <- byName.keys.toSeq.sorted) visit(node)
    result.result()
  }

  /** List all definitions used by an [[Expression]], also called the expression's subdefinitions. */
  def subdefinitions(of: Expression): Set[Name] = StaticSemantics.signature(of).map(Name(_)).filter(byName.contains)

  /** List all definitions used by a [[Sequent]], also called the sequent's subdefinitions. */
  def subdefinitions(of: Sequent): Set[Name] = StaticSemantics.signature(of).map(Name(_)).filter(byName.contains)

  /** List all definitions used by a [[Provable]], also called the provable's subdefinitions. */
  def subdefinitions(of: Provable): Set[Name] = of
    .subgoals
    .map(subdefinitions)
    .fold(subdefinitions(of.conclusion))(_ ++ _)

  /** The set of definitions that are transitively reachable from a starting set of definitions. */
  def reachable(from: Set[Name]): Set[Name] = {
    val visited = mutable.Set.empty[Name]

    def visit(node: Name): Unit = {
      if (visited.contains(node)) return
      visited.add(node)
      for (child <- subdefsByName(node)) visit(child)
    }

    for (node <- from) visit(node)
    visited.toSet
  }

  def reachable(from: Expression): Set[Name] = reachable(subdefinitions(from))
  def reachable(from: Sequent): Set[Name] = reachable(subdefinitions(from))
  def reachable(from: Provable): Set[Name] = reachable(subdefinitions(from))

  def retainReachable(from: Set[Name]): Definitions = {
    val reachableNames = reachable(from)
    if (reachableNames == names) return this // For efficiency
    Definitions(byName.filter { case (name, _) => reachableNames.contains(name) })
  }

  def retainReachable(from: Expression): Definitions = retainReachable(subdefinitions(from))
  def retainReachable(from: Sequent): Definitions = retainReachable(subdefinitions(from))
  def retainReachable(from: Provable): Definitions = retainReachable(subdefinitions(from))

  def merge(other: Definitions): Definitions = {
    for {
      (name, repl) <- byName
      otherRepl <- other.byName.get(name)
    } require(repl == otherRepl, s"replacements for $name differ")

    Definitions(byName ++ other.byName)
  }

  def applyRename(rename: URename): Definitions = {
    require(subdefinitions(rename.what).isEmpty)
    require(subdefinitions(rename.repl).isEmpty)
    Definitions(byName.map { case (name, repl) => (name, repl.applyRename(rename)) })
  }

  def applySubst(subst: USubst): Definitions = {
    // I don't believe it is necessary to restrict pair.repl in the same way, but I could be wrong.
    for (pair <- subst.subsDefsInput) require(subdefinitions(pair.what).isEmpty)
    Definitions(byName.map { case (name, repl) => (name, repl.applySubst(subst)) })
  }

  def applySubstAllTaboo(subst: USubst): Definitions = {
    // I don't believe it is necessary to restrict pair.repl in the same way, but I could be wrong.
    for (pair <- subst.subsDefsInput) require(subdefinitions(pair.what).isEmpty)
    Definitions(byName.map { case (name, repl) => (name, repl.applySubstAllTaboo(subst)) })
  }

  private def expandingSubstPair(name: Name): SubstitutionPair = {
    val repl = byName(name)
    SubstitutionPair(repl.placeholder(name), repl.expr)
  }

  private def expandingSubst(names: Name*): USubst = USubst(names.map(expandingSubstPair))

  def expand(name: Name, expr: Expression): Expression = expandingSubst(name)(expr)
  def expand(name: Name, sequent: Sequent): Sequent = expandingSubst(name)(sequent)
  def expand(name: Name, provable: Provable): Provable = provable(expandingSubst(name))

  // I don't think the topological order is necessary since USubst substitutes "all at once",
  // but Declaration seems to do the same, and we already have it computed,
  // and it is more deterministic than using a random key order.
  private def expandAllOnce(expr: Expression): Expression = expandingSubst(topologically*)(expr)
  private def expandAllOnce(sequent: Sequent): Sequent = expandingSubst(topologically*)(sequent)
  private def expandAllOnce(provable: Provable): Provable = provable(expandingSubst(topologically*))

  def expandAll(expr: Expression): Expression = {
    if (byName.isEmpty) return expr
    val newExpr = expandAllOnce(expr)
    retainReachable(newExpr).expandAll(newExpr)
  }

  def expandAll(sequent: Sequent): Sequent = {
    if (byName.isEmpty) return sequent
    val newSequent = expandAllOnce(sequent)
    retainReachable(newSequent).expandAll(newSequent)
  }

  def expandAll(provable: Provable): Provable = {
    if (byName.isEmpty) return provable
    val newProvable = expandAllOnce(provable)
    retainReachable(newProvable).expandAll(newProvable)
  }

  override def digestInto(hasher: Hasher): Unit = hasher.digestMap(byName)
}

object Definitions {

  /** An empty set of definitions. */
  def empty: Definitions = Definitions(SortedMap.empty)
}
