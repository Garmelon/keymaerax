/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.hippocore.{BackwardTactic, ForwardTactic, PureTactic, Tactic}

object Cached {
  def apply(tactic: PureTactic): PureTactic = CachedPure(tactic)

  def apply(tactic: ForwardTactic): ForwardTactic = tactic match {
    case tactic: PureTactic => CachedPure(tactic)
    case tactic: BackwardTactic => CachedBidi(tactic)
    case tactic => CachedForward(tactic)
  }

  def apply(tactic: BackwardTactic): BackwardTactic = tactic match {
    case tactic: PureTactic => CachedPure(tactic)
    case tactic: ForwardTactic => CachedBidi(tactic)
    case tactic => CachedBackward(tactic)
  }

  def apply(tactic: Tactic): Tactic = tactic match {
    case tactic: PureTactic => CachedPure(tactic)
    case tactic: ForwardTactic with BackwardTactic => CachedBidi(tactic)
    case tactic: ForwardTactic => CachedForward(tactic)
    case tactic: BackwardTactic => CachedBackward(tactic)
  }
}
