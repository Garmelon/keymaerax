/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.proof

import org.keymaerax.core.Sequent
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.Hash
import org.keymaerax.hippolochos.{PureTactic, Tactic}

import scala.collection.mutable

/**
 * A derived proof is a proof that can be derived from other proofs by the use of tactics.
 *
 * Each derived proof must have a globally unique shape, i.e. the combination of conclusion and premises must be
 * globally unique. A hash of the shape is available for caching purposes.
 *
 * @see
 *   [[HippoContext.derived]]
 */
case class DerivedHippoProof(conclusion: Sequent, premises: IndexedSeq[HippoPremise], by: Tactic) {
  val hash: Hash = Hash.ofProofShape(conclusion, premises)

  // To ensure global uniqueness, we register with a global registry of all known hashes.
  // This way, construction of the derived proof will fail if it is not unique.
  DerivedHippoProof.registerHash(hash)
}

object DerivedHippoProof {
  private val registeredHashes = mutable.Set[Hash]()
  private def registerHash(hash: Hash): Unit = synchronized {
    require(!registeredHashes.contains(hash), s"DerivedHippoProof hash already registered: $hash")
    registeredHashes.add(hash)
  }

  def apply(conclusion: Sequent, premises: HippoPremise*)(by: HippoContext => HippoProof): DerivedHippoProof = {
    val byPure: PureTactic = by(_)
    DerivedHippoProof(conclusion = conclusion, premises = premises.toIndexedSeq, by = byPure)
  }
}
