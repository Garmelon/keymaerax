/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.tools

import org.keymaerax.hippolochos.tools.GloballyUniqueName.knownNames

import scala.collection.mutable

case class GloballyUniqueName(name: String) {
  synchronized {
    require(!knownNames.contains(name), s"name is already in use: $name")
    knownNames.add(name)
  }
}

object GloballyUniqueName {
  private val knownNames: mutable.Set[String] = mutable.Set.empty
}
