/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.cache

import org.keymaerax.core.Provable

import java.nio.file.Path

class ProvableFsCache(dir: Path) extends FsCache[Provable](dir, version = 0) {
  override def format(value: Provable): String = Provable.toStorageString(value)
  override def parse(str: String): Provable = Provable.fromStorageString(str)
}
