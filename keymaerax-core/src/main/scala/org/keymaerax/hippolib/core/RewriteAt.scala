/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.{
  CoHideRight,
  CommuteEquivRight,
  CutRight,
  Equal,
  Equiv,
  EquivifyRight,
  Expression,
  Formula,
  Sequent,
  SuccPos,
}
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{ExprPath, Hash, Hasher}
import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic}
import org.keymaerax.hippolib.HippoLib
import org.keymaerax.hippolib.primitive.Noop

/**
 * Rewrite a formula at a single location using an equivalence (`<->`) or equals (`=`) relation.
 *
 * Defaults to [[RewriteAt.Dir.Rtl]] if the direction is otherwise ambiguous.
 *
 * Schema (assuming [[RewriteAt.Dir.Ltr]]):
 * {{{
 *    G ⊢ C{left}    ⊢ left<->right
 *   -------------------------------
 *            G ⊢ C{right}
 * }}}
 * {{{
 *    G ⊢ C(left)    ⊢ left=right
 *   -----------------------------
 *           G ⊢ C(right)
 * }}}
 */
case class RewriteAt(at: ExprPath, dir: Option[RewriteAt.Dir] = None)(implicit lib: HippoLib)
    extends ForwardTactic with BackwardTactic {
  import RewriteAt.Dir

  override lazy val hash: Hash = Hasher()
    .digest[this.type]
    .digest(at)
    .digestOptWith(dir) {
      case (b, Dir.Ltr) => b.digest("Ltr")
      case (b, Dir.Rtl) => b.digest("Rtl")
    }
    .hash

  // Rough sketch of proof structure
  // where eqF is either leftE=rightE or leftE<->rightE:
  //
  // §                        ⊢ eqF
  // §                  ---------------- CE/CQ
  // §                  ⊢ leftF<->rightF
  // §                 ------------------ CommuteEquivRight depending on dir
  // §                 ⊢ beforeF<->afterF
  // §                -------------------- CoHideRight
  // §                G ⊢ beforeF<->afterF
  // §                -------------------- EquivifyRight
  // §  G ⊢ beforeF    G ⊢ beforeF->afterF
  // §  ---------------------------------- CutRight
  // §              G ⊢ afterF

  private def buildProof(
      ctx: HippoContext,
      dir: Dir,
      g: IndexedSeq[Formula],
      beforeF: Formula,
      afterF: Formula,
  ): HippoProof = ctx
    .chain(HippoSequent(Sequent(ante = g, succ = IndexedSeq(afterF))))
    .backward(CoreRule(CutRight(beforeF, SuccPos(0))))
    .backwardAt(1)(CoreRule(EquivifyRight(SuccPos(0))))
    .backwardAt(1)(CoreRule(CoHideRight(SuccPos(0))))
    .backwardAt(1)(dir match {
      case Dir.Ltr => Noop
      case Dir.Rtl => CoreRule(CommuteEquivRight(SuccPos(0)))
    })
    .backwardAt(1)(CEqAt(at))
    .proof

  private def fromBeforeAndAfter(ctx: HippoContext, before: HippoSequent, after: HippoSequent): HippoProof = {
    require(before.sequent.succ.length == 1)
    require(after.sequent.succ.length == 1)
    require(before.sequent.ante == after.sequent.ante)

    val g = before.sequent.ante
    val Seq(beforeF) = before.sequent.succ
    val Seq(afterF) = after.sequent.succ
    val actualDir = dir.getOrElse(Dir.Rtl)

    buildProof(ctx, actualDir, g, beforeF, afterF)
  }

  private def fromBeforeAndEq(ctx: HippoContext, before: HippoSequent, eq: HippoSequent): HippoProof = {
    require(before.sequent.succ.length == 1)
    require(eq.sequent.ante.isEmpty)
    require(eq.sequent.succ.length == 1)

    val g = before.sequent.ante
    val Seq(beforeF) = before.sequent.succ
    val Seq(eqF) = eq.sequent.succ

    val (leftE: Expression, rightE: Expression) = eqF match {
      case f: Equal => (f.left, f.right)
      case f: Equiv => (f.left, f.right)
      case _ => ???
    }

    val beforeInner = at.select(beforeF)
    val actualDir = dir match {
      case Some(Dir.Rtl) | None if beforeInner == rightE => Dir.Rtl
      case Some(Dir.Ltr) | None if beforeInner == leftE => Dir.Ltr
      case _ => ???
    }

    val afterF = actualDir match {
      case Dir.Rtl => at.replace(beforeF, leftE)
      case Dir.Ltr => at.replace(beforeF, rightE)
    }

    buildProof(ctx, actualDir, g, beforeF, afterF)
  }

  private def fromAfterAndEq(ctx: HippoContext, after: HippoSequent, eq: HippoSequent): HippoProof = {
    require(after.sequent.succ.length == 1)
    require(eq.sequent.ante.isEmpty)
    require(eq.sequent.succ.length == 1)

    val g = after.sequent.ante
    val Seq(afterF) = after.sequent.succ
    val Seq(eqF) = eq.sequent.succ

    val (leftE: Expression, rightE: Expression) = eqF match {
      case f: Equal => (f.left, f.right)
      case f: Equiv => (f.left, f.right)
      case _ => ???
    }

    val afterInner = at.select(afterF)
    val actualDir = dir match {
      case Some(Dir.Rtl) | None if afterInner == leftE => Dir.Rtl
      case Some(Dir.Ltr) | None if afterInner == rightE => Dir.Ltr
      case _ => ???
    }

    val beforeF = actualDir match {
      case Dir.Rtl => at.replace(afterF, rightE)
      case Dir.Ltr => at.replace(afterF, leftE)
    }

    buildProof(ctx, actualDir, g, beforeF, afterF)
  }

  override def runForward(ctx: HippoContext, premises: IndexedSeq[HippoSequent]): HippoProof = {
    require(premises.length == 2, "exactly two premises required")
    val Seq(before, eq) = premises
    fromBeforeAndEq(ctx, before, eq)
  }

  override def runBackward(
      ctx: HippoContext,
      conclusion: HippoSequent,
      premises: Map[Int, HippoSequent],
  ): HippoProof = {
    require(premises.contains(0) || premises.contains(1), "premise 0 or 1 required")
    if (premises.contains(0)) {
      val before = premises(0)
      fromBeforeAndAfter(ctx, before, conclusion)
    } else {
      val eq = premises(1)
      fromAfterAndEq(ctx, conclusion, eq)
    }
  }
}

object RewriteAt {
  sealed trait Dir
  object Dir {
    case object Ltr extends Dir
    case object Rtl extends Dir

    def parse(s: String): Dir = s match {
      case "ltr" => Dir.Ltr
      case "rtl" => Dir.Rtl
      case other => throw new IllegalArgumentException(s"unknown direction: $other")
    }
  }
}
