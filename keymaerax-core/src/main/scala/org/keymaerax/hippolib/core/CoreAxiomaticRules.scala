/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.hippolib.publish
import org.keymaerax.hippolib.meta.ProofInfo

/** Rules from [[org.keymaerax.core.Provable.rules]]. */
// TODO Check if the list is complete
// TODO Doc comments for every rule
object CoreAxiomaticRules {
  @publish(name = "core.CQrule")
  val CQrule: ProofInfo = ProofInfo.coreAxiomaticRule("CQ equation congruence")

  @publish(name = "core.CErule")
  val CErule: ProofInfo = ProofInfo.coreAxiomaticRule("CE congruence")

  @publish(name = "core.mondrule")
  val mondrule: ProofInfo = ProofInfo.coreAxiomaticRule("<> monotone")

  @publish(name = "core.FPrule")
  val FPrule: ProofInfo = ProofInfo.coreAxiomaticRule("FP fixpoint")

  @publish(name = "core.conrule")
  val conrule: ProofInfo = ProofInfo.coreAxiomaticRule("con convergence")
}
