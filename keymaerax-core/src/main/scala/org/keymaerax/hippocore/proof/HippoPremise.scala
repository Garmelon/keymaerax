/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.proof

import org.keymaerax.hippocore.tools.{Hashable, Hasher}

/**
 * The premise of a [[HippoProof]].
 *
 * @param mustBeProved
 *   This premise must be proved for the proof to be locally sound.
 */
case class HippoPremise(sequent: HippoSequent, mustBeProved: Boolean) extends Hashable {
  @inline
  def mapSequent(f: HippoSequent => HippoSequent): HippoPremise = copy(sequent = f(sequent))

  override def digestInto(hasher: Hasher): Unit = hasher.digest(sequent).digest(mustBeProved)
}

object HippoPremise {
  def locallySound(sequent: HippoSequent): HippoPremise = new HippoPremise(sequent, mustBeProved = false)
  def locallyUnsound(sequent: HippoSequent): HippoPremise = new HippoPremise(sequent, mustBeProved = true)
}
