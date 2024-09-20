/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.namespace

import org.keymaerax.hippolang.{HippoIdentifier, HippoValue}

final case class ImmutableNamespace(variables: Map[HippoIdentifier, HippoValue], child: Option[ImmutableNamespace])
    extends Namespace {
  override def declare(name: HippoIdentifier, value: HippoValue, mutable: Boolean): Unit =
    throw new UnsupportedOperationException("immutable namespace can't declare")

  override def assign(name: HippoIdentifier, value: HippoValue): Unit =
    throw new UnsupportedOperationException("immutable namespace can't assign")

  override def lookup(name: HippoIdentifier): HippoValue = lookupOpt(name)
    .getOrElse(throw new IllegalArgumentException(s"variable $name does not exist"))

  override def lookupOpt(name: HippoIdentifier): Option[HippoValue] = variables
    .get(name)
    .orElse(child.flatMap(_.lookupOpt(name)))

  override def freeze: ImmutableNamespace = this
}
