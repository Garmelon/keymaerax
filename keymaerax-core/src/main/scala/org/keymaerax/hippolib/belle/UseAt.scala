/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.belle

import org.keymaerax.bellerophon.{BelleInterpreter, BelleProvable}
import org.keymaerax.btactics.UnifyUSCalculus
import org.keymaerax.core.{Provable, SeqPos, Sequent}
import org.keymaerax.hippolib.meta.ProofInfo
import org.keymaerax.hippolochos.BackwardTactic
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.{ExprPath, Hash, Hasher}
import org.keymaerax.infrastruct.{PosInExpr, Position}
import org.keymaerax.lemma.Lemma
import org.keymaerax.parser.Declaration
import org.keymaerax.pt.ElidingProvable

/**
 * Call [[UnifyUSCalculus.useAt]].
 *
 * @param proof
 *   The axiom to use via unification.
 * @param sequent
 *   The sequent to apply the proof to.
 * @param path
 *   Where in the sequent to apply the proof.
 * @param key
 *   The optional position of the key in the axiom to unify with.
 */
case class UseAt(proof: ProofInfo, sequent: SeqPos = SeqPos(1), path: ExprPath = ExprPath(), key: ExprPath = ExprPath())
    extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(proof).digest(sequent).digest(path).digest(key).hash

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = {
    val belleLemma = {
      val provable = ctx.provableFromLocalProof(proof.proof)
      val provableSig = ElidingProvable(provable, Declaration(Map.empty))
      Lemma(provableSig, Nil)
    }
    val belleKey = PosInExpr(key.segments)
    val belleAt = Position(sequent.getPos, path.segments)

    val startProvable = Provable.startProof(conclusion)
    val startProvableSig = ElidingProvable(startProvable, Declaration(Map.empty))

    val tactic = UnifyUSCalculus.useAt(belleLemma, belleKey)(belleAt)
    val resultValue = BelleInterpreter(tactic, BelleProvable(startProvableSig, None))
    val resultProvable = resultValue match {
      case BelleProvable(p, _) => p.underlyingProvable
      case _ => ???
    }

    ctx.belle(resultProvable)
  }
}
