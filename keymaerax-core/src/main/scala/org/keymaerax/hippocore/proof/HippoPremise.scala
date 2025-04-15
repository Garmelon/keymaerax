/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.proof

import org.keymaerax.core.Sequent
import org.keymaerax.hippocore.tools.{Hashable, Hasher}

/**
 * The premise of a [[HippoProof]].
 *
 * @param mustBeProved
 *   This premise must be proved for the proof to be locally sound.
 */
case class HippoPremise(sequent: Sequent, mustBeProved: Boolean) extends Hashable {
  override def digestInto(hasher: Hasher): Unit = hasher.digest(sequent).digest(mustBeProved)
}

object HippoPremise {
  def locallySound(sequent: Sequent): HippoPremise = new HippoPremise(sequent, mustBeProved = false)
  def locallyUnsound(sequent: Sequent): HippoPremise = new HippoPremise(sequent, mustBeProved = true)
}
