/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.cache

import org.keymaerax.hippocore.tools.Hash

class ChainCache[V](val first: Cache[V], val second: Cache[V]) extends Cache[V] {
  override def get(key: Hash): Option[V] = first.get(key).orElse(second.get(key))

  override def put(key: Hash, value: V): Unit = {
    second.put(key, value)
    first.put(key, value)
  }

  override def clear(): Unit = {
    second.clear()
    first.clear()
  }
}
