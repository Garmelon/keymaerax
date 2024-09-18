/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.hippolochos.proof.HippoProof

/** Rules from [[org.keymaerax.core.Provable.rules]]. */
// TODO Check if the list is complete
// TODO Doc comments for every rule
object CoreAxiomaticRules {
  val CQrule: HippoProof.CoreAxiomaticRule = HippoProof.CoreAxiomaticRule("CQ equation congruence")
  val CErule: HippoProof.CoreAxiomaticRule = HippoProof.CoreAxiomaticRule("CE congruence")
  val mondrule: HippoProof.CoreAxiomaticRule = HippoProof.CoreAxiomaticRule("<> monotone")
  val FPrule: HippoProof.CoreAxiomaticRule = HippoProof.CoreAxiomaticRule("FP fixpoint")
  val conrule: HippoProof.CoreAxiomaticRule = HippoProof.CoreAxiomaticRule("con convergence")
}
