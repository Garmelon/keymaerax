/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

import org.keymaerax.hippolochos.proof.{DerivedHippoProof, HippoProof}
import org.keymaerax.hippolochos.run.HippoContext

case class ProofInfo(proof: HippoContext => HippoProof) {
  @inline
  def apply(ctx: HippoContext): HippoProof = proof(ctx)
}

object ProofInfo {
  def coreAxiom(name: String): ProofInfo = ProofInfo { _.coreAxiom(name) }

  def coreAxiomaticRule(name: String): ProofInfo = ProofInfo { _.coreAxiomaticRule(name) }

  def derived(proof: DerivedHippoProof): ProofInfo = ProofInfo { _.derived(proof) }
}
