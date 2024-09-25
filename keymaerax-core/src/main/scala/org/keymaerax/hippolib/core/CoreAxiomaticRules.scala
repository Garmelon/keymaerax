/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.hippolib.meta.ProofInfo

/** Rules from [[org.keymaerax.core.Provable.rules]]. */
// TODO Check if the list is complete
// TODO Doc comments for every rule
object CoreAxiomaticRules {
  val CQrule: ProofInfo = ProofInfo.coreAxiomaticRule("CQ equation congruence")
  val CErule: ProofInfo = ProofInfo.coreAxiomaticRule("CE congruence")
  val mondrule: ProofInfo = ProofInfo.coreAxiomaticRule("<> monotone")
  val FPrule: ProofInfo = ProofInfo.coreAxiomaticRule("FP fixpoint")
  val conrule: ProofInfo = ProofInfo.coreAxiomaticRule("con convergence")
}
