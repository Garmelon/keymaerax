/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

import scala.collection.mutable

class UniqueNameRegistry {
  private val knownNames: mutable.Set[String] = mutable.Set.empty

  def register(name: String): Unit = synchronized {
    require(!knownNames.contains(name), s"name is already in use: $name")
    knownNames.add(name)
  }
}
