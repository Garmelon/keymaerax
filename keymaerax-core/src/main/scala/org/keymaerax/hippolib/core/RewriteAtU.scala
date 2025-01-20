/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.bellerophon.UnificationException
import org.keymaerax.core.{Equal, Equiv, Expression, Sequent}
import org.keymaerax.hippolib.HippoLib
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.{ExprPath, Hash, Hasher}
import org.keymaerax.hippolochos.{BackwardTactic, ForwardTactic}
import org.keymaerax.infrastruct.UnificationMatch

/**
 * Rewrite a formula at a single location using an equivalence (`<->`) or equals (`=`) relation using unification.
 *
 * Defaults to [[RewriteAt.Dir.Rtl]] if the direction is otherwise ambiguous.
 */
case class RewriteAtU(at: ExprPath, eq: HippoProof, dir: Option[RewriteAt.Dir] = None)(implicit lib: HippoLib)
    extends ForwardTactic with BackwardTactic {
  import RewriteAt.Dir

  override lazy val hash: Hash = Hasher()
    .digest[this.type]
    .digest(at)
    .digest(eq)
    .digestOptWith(dir) {
      case (b, Dir.Ltr) => b.digest("Ltr")
      case (b, Dir.Rtl) => b.digest("Rtl")
    }
    .hash

  require(eq.proved)
  require(eq.conclusion.ante.isEmpty)
  require(eq.conclusion.succ.length == 1)

  private val Seq(eqF) = eq.conclusion.succ
  private val (leftE: Expression, rightE: Expression) = eqF match {
    case f: Equal => (f.left, f.right)
    case f: Equiv => (f.left, f.right)
    case _ => ???
  }

  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = {
    require(premises.length == 1, "exactly one premise required")
    val Seq(before) = premises

    require(before.succ.length == 1)
    val Seq(beforeF) = before.succ

    val beforeInner = at.select(beforeF)
    val (actualDir, subst) = dir match {
      case Some(Dir.Rtl) => (Dir.Rtl, UnificationMatch(rightE, beforeInner))
      case Some(Dir.Ltr) => (Dir.Ltr, UnificationMatch(leftE, beforeInner))
      case None =>
        try (Dir.Rtl, UnificationMatch(rightE, beforeInner))
        catch { case _: UnificationException => (Dir.Ltr, UnificationMatch(leftE, beforeInner)) }
    }

    ctx.chain(before).forwardJoin(RewriteAt(at, dir = Some(actualDir)), subst.toHippo(eq)).proof
  }

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = {
    val after = conclusion

    require(after.succ.length == 1)
    val Seq(afterF) = after.succ

    val afterInner = at.select(afterF)
    val (actualDir, subst) = dir match {
      case Some(Dir.Rtl) => (Dir.Rtl, UnificationMatch(leftE, afterInner))
      case Some(Dir.Ltr) => (Dir.Ltr, UnificationMatch(rightE, afterInner))
      case None =>
        try (Dir.Rtl, UnificationMatch(leftE, afterInner))
        catch { case _: UnificationException => (Dir.Ltr, UnificationMatch(rightE, afterInner)) }
    }

    ctx.chain(after).backwardJoin(RewriteAt(at, dir = Some(actualDir)), 1 -> subst.toHippo(eq)).proof
  }
}
