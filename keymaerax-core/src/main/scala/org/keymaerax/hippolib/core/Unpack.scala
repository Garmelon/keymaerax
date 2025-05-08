/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.{
  And,
  AndLeft,
  AntePos,
  False,
  Formula,
  HideRight,
  Imply,
  ImplyRight,
  Or,
  OrRight,
  Sequent,
  SuccPos,
}
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.{HippoContext, ProofChain}
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic}
import org.keymaerax.hippolib.primitive.PureBackward

import scala.util.control.Breaks.{break, breakable}

case object Unpack extends BackwardTactic with ForwardTactic {
  override def hash: Hash = Hasher().digest[this.type].hash

  private def partialFind(fmls: Seq[Formula], pred: PartialFunction[Formula, Boolean]): Option[Int] = fmls
    .zipWithIndex
    .find { case (fml, _) => pred.applyOrElse[Formula, Boolean](fml, _ => false) }
    .map { case (_, i) => i }

  private def succFind(chain: ProofChain)(pred: PartialFunction[Formula, Boolean]): Option[SuccPos] =
    partialFind(chain.proof.premises.head.sequent.sequent.succ, pred).map(SuccPos(_))

  private def anteFind(chain: ProofChain)(pred: PartialFunction[Formula, Boolean]): Option[AntePos] =
    partialFind(chain.proof.premises.head.sequent.sequent.ante, pred).map(AntePos(_))

  override def runBackward(
      ctx: HippoContext,
      conclusion: HippoSequent,
      premises: Map[Int, HippoSequent],
  ): HippoProof = {
    var chain = ctx.chain(conclusion)
    var loop = true
    while (loop) breakable {
      // Break up -> on succ side
      for (pos <- succFind(chain) { case _: Imply => true }) {
        chain = chain.backward(CoreRule(ImplyRight(pos)))
        break()
      }

      // Break up | on succ side
      for (pos <- succFind(chain) { case _: Or => true }) {
        chain = chain.backward(CoreRule(OrRight(pos)))
        break()
      }

      // Break up & on ante side
      for (pos <- anteFind(chain) { case _: And => true }) {
        chain = chain.backward(CoreRule(AndLeft(pos)))
        break()
      }

      loop = false
    }
    chain.proof
  }

  override def runForward(ctx: HippoContext, premises: IndexedSeq[HippoSequent]): HippoProof = {
    val Seq(premise) = premises
    var chain = ctx.chain(premise)
    var loop = true
    while (loop) breakable {
      val conc = chain.proof.conclusion

      // Join succs with | until there's at most one succ left
      if (conc.sequent.succ.length > 1) {
        val Seq(succ1, succ2) = conc.sequent.succ.takeRight(2)
        val sequent = conc.sequent.copy(succ = conc.sequent.succ.dropRight(2) :+ Or(succ1, succ2))
        val pos = SuccPos(sequent.succ.length - 1)
        chain = chain.forward(PureBackward(CoreRule(OrRight(pos)), HippoSequent(sequent, conc.defs)))
        break()
      }

      // Join antes with & until there's at most one ante left
      if (conc.sequent.ante.length > 1) {
        val Seq(ante1, ante2) = conc.sequent.ante.takeRight(2)
        val sequent = conc.sequent.copy(ante = conc.sequent.ante.dropRight(2) :+ And(ante1, ante2))
        val pos = AntePos(sequent.ante.length - 1)
        chain = chain.forward(PureBackward(CoreRule(AndLeft(pos)), HippoSequent(sequent, conc.defs)))
        break()
      }

      // Add a False succ if succs are otherwise empty
      if (conc.sequent.succ.isEmpty) {
        val sequent = conc.sequent.copy(succ = IndexedSeq(False))
        chain = chain.forward(PureBackward(CoreRule(HideRight(SuccPos(0))), HippoSequent(sequent, conc.defs)))
        break()
      }

      // Move the single ante that's left over to the succ side using ->
      if (conc.sequent.ante.nonEmpty) {
        val Seq(ante) = conc.sequent.ante
        val Seq(succ) = conc.sequent.succ
        val sequent = Sequent(ante = IndexedSeq(), succ = IndexedSeq(Imply(ante, succ)))
        chain = chain.forward(PureBackward(CoreRule(ImplyRight(SuccPos(0))), HippoSequent(sequent, conc.defs)))
        break()
      }

      loop = false
    }
    chain.proof
  }
}
