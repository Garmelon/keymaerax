/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.proof

import org.keymaerax.core.Sequent

/** The premise of a [[HippoProof]]. */
case class HippoPremise(
    sequent: Sequent,

    /** This premise must be proved for the proof to be locally sound. */
    mustBeProved: Boolean,
)

object HippoPremise {
  def locallySound(sequent: Sequent): HippoPremise = new HippoPremise(sequent, mustBeProved = false)
  def locallyUnsound(sequent: Sequent): HippoPremise = new HippoPremise(sequent, mustBeProved = true)
}
