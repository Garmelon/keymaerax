/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.definitions

import org.keymaerax.core
import org.keymaerax.core.{Formula, URename, USubst}
import org.keymaerax.hippocore.tools.{Hashable, Hasher}

sealed trait Replacement extends Hashable {
  def expr: core.Expression
  def placeholder(name: Name): core.Expression

  def applyRename(rename: URename): Replacement
  def applySubst(subst: USubst): Replacement
  def applySubstAllTaboo(subst: USubst): Replacement
}

object Replacement {
  case class BaseVariable(expr: core.Term) extends Replacement {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(expr)
    override def placeholder(name: Name): core.Expression = core.BaseVariable(name.name, name.index)

    override def applyRename(rename: URename): Replacement = copy(expr = rename(expr))
    override def applySubst(subst: USubst): Replacement = copy(expr = subst.apply(expr))
    override def applySubstAllTaboo(subst: USubst): Replacement = copy(expr = subst.applyAllTaboo(expr))
  }

  case class FuncOf(args: Int, expr: core.Term) extends Replacement {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(args).digest(expr)
    override def placeholder(name: Name): core.Expression = core.FuncOf(
      func = core.Function(name = name.name, index = name.index, domain = realArgSorts(args), sort = core.Real),
      child = realArgDots(args),
    )

    override def applyRename(rename: URename): Replacement = copy(expr = rename(expr))
    override def applySubst(subst: USubst): Replacement = copy(expr = subst.apply(expr))
    override def applySubstAllTaboo(subst: USubst): Replacement = copy(expr = subst.applyAllTaboo(expr))
  }

  case class PredOf(args: Int, expr: core.Formula) extends Replacement {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(args).digest(expr)
    override def placeholder(name: Name): core.Expression = core.PredOf(
      func = core.Function(name = name.name, index = name.index, domain = realArgSorts(args), sort = core.Bool),
      child = realArgDots(args),
    )

    override def applyRename(rename: URename): Replacement = copy(expr = rename(expr))
    override def applySubst(subst: USubst): Replacement = copy(expr = subst.apply(expr))
    override def applySubstAllTaboo(subst: USubst): Replacement = copy(expr = subst.applyAllTaboo(expr))
  }

  case class PredicationalOf(expr: core.Formula) extends Replacement {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(expr)
    override def placeholder(name: Name): core.Expression = core.PredicationalOf(
      func = core.Function(name = name.name, index = name.index, domain = core.Bool, sort = core.Bool),
      child = core.DotFormula,
    )

    override def applyRename(rename: URename): Replacement = copy(expr = rename(expr))
    override def applySubst(subst: USubst): Replacement = copy(expr = subst.apply(expr))
    override def applySubstAllTaboo(subst: USubst): Replacement = copy(expr = subst.applyAllTaboo(expr))
  }

  case class ProgramConst(expr: core.Program) extends Replacement {
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(expr)
    override def placeholder(name: Name): core.Expression = {
      require(name.index.isEmpty)
      core.ProgramConst(name.name)
    }

    override def applyRename(rename: URename): Replacement = copy(expr = rename(expr))
    override def applySubst(subst: USubst): Replacement = copy(expr = subst.apply(expr))
    override def applySubstAllTaboo(subst: USubst): Replacement = copy(expr = subst.applyAllTaboo(expr))
  }

  private def foldArgSorts(sorts: List[core.Sort]): core.Sort = sorts match {
    case Nil => core.Unit
    case ::(head, Nil) => head
    case ::(head, next) => core.Tuple(head, foldArgSorts(next))
  }

  private def foldArgTerms(terms: List[core.Term]): core.Term = terms match {
    case Nil => core.Nothing
    case ::(head, Nil) => head
    case ::(head, next) => core.Pair(head, foldArgTerms(next))
  }

  private def realArgSorts(n: Int): core.Sort = foldArgSorts(List.fill(n)(core.Real))
  private def realArgDots(n: Int): core.Term = foldArgTerms((0 until n).map(i => core.DotTerm(idx = Some(i))).toList)
}
