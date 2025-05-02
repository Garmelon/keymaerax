/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.namespace

import org.keymaerax.hippocore.tools.{Hash, Hashable, Hasher}
import org.keymaerax.hippolang.{HlangIdentifier, HlangValue}

final case class ImmutableNamespace(variables: Map[HlangIdentifier, HlangValue], child: Option[ImmutableNamespace])
    extends Namespace with Hashable {

  override def declare(name: HlangIdentifier, value: HlangValue, mutable: Boolean): Unit =
    throw new UnsupportedOperationException("immutable namespace can't declare")

  override def assign(name: HlangIdentifier, value: HlangValue): Unit =
    throw new UnsupportedOperationException("immutable namespace can't assign")

  override def lookup(name: HlangIdentifier): HlangValue = lookupOpt(name)
    .getOrElse(throw new IllegalArgumentException(s"variable $name does not exist"))

  override def lookupOpt(name: HlangIdentifier): Option[HlangValue] = variables
    .get(name)
    .orElse(child.flatMap(_.lookupOpt(name)))

  override def freeze: ImmutableNamespace = this

  /////////////
  // Hashing //
  /////////////

  lazy val hash: Hash = Hasher()
    .digestSeqWith(variables.toSeq.sortBy(_._1)) { case (hasher, (k, v)) => hasher.digest(k).digest(v) }
    .digestOpt(child)
    .hash

  override def digestInto(hasher: Hasher): Unit = hasher.digest(hash)
}
