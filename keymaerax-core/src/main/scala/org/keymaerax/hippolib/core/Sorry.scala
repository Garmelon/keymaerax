/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.{Sequent, True}
import org.keymaerax.hippolochos.proof.{HippoPremise, HippoProof}
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.{Hash, Hasher}
import org.keymaerax.hippolochos.{BackwardTactic, ForwardTactic}

import scala.collection.SortedMap

case class Sorry(conclusion: Option[Sequent] = None, premises: SortedMap[Int, Sequent] = SortedMap.empty)
    extends ForwardTactic with BackwardTactic {

  override lazy val hash: Hash = Hasher()
    .digest[this.type]
    .digestOptWith(conclusion)(_.digest(_))
    .digestMapWith(premises)(_.digest(_).digest(_))
    .hash

  override def runForward(ctx: HippoContext, premises: IndexedSeq[Sequent]): HippoProof = {
    // TODO Nicer error handling
    require(this.conclusion.isDefined)
    val conclusion = this.conclusion.get
    for ((i, premise) <- this.premises) require(premises(i) == premise)
    ctx.sorry(conclusion, premises.map(HippoPremise.locallySound))
  }

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = {
    // TODO Nicer error handling
    for (conc <- this.conclusion) require(conclusion == conc)
    for ((i, premise) <- this.premises) require(premises(i) == premise)
    val allPremises = this.premises ++ premises
    val maxI = allPremises.keys.maxOption.getOrElse(-1)
    val trueSequent = Sequent(IndexedSeq(), IndexedSeq(True)) // "==> true"
    val premisesList = (0 to maxI).map(allPremises.getOrElse(_, trueSequent))
    ctx.sorry(conclusion, premisesList.map(HippoPremise.locallySound))
  }
}
