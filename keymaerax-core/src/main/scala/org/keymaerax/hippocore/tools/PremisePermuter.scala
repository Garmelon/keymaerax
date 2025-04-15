/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.tools

import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext

object PremisePermuter {
  // TODO Use this in ProofGraph
  def permute(ctx: HippoContext, proof: HippoProof, targetIndices: IndexedSeq[Int]): HippoProof = {
    require(targetIndices.length == proof.premises.length)
    require(targetIndices.toSet == proof.premises.indices.toSet)

    val indices = targetIndices.toBuffer
    var result = proof

    // Selection sort from right to left.
    // Should result in the minimal number of swaps, but it's O(n²).
    while (indices.nonEmpty) {
      val i = indices.length - 1
      val j = indices.indexOf(i)
      if (i != j) {
        result = ctx.swap(result, i, j)
        indices(j) = indices(i)
        // indices(i) = i
      }
      indices.dropRightInPlace(1)
    }

    result
  }
}
