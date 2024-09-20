/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.namespace

import org.keymaerax.hippolang.{HippoIdentifier, HippoValue}

trait Namespace {
  def declare(name: HippoIdentifier, value: HippoValue, mutable: Boolean): Unit
  def assign(name: HippoIdentifier, value: HippoValue): Unit
  def lookup(name: HippoIdentifier): HippoValue
  def lookupOpt(name: HippoIdentifier): Option[HippoValue]

  def freeze: ImmutableNamespace
}
