/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.namespace

import org.keymaerax.hippolang.{HlangIdentifier, HlangValue}

trait Namespace {
  def declare(name: HlangIdentifier, value: HlangValue, mutable: Boolean): Unit
  def assign(name: HlangIdentifier, value: HlangValue): Unit
  def lookup(name: HlangIdentifier): HlangValue
  def lookupOpt(name: HlangIdentifier): Option[HlangValue]

  def freeze: ImmutableNamespace
}
