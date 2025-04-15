/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.cache

import org.keymaerax.hippocore.tools.Hash

import scala.collection.mutable

class LruCache[V](val capacity: Int = 1000) extends Cache[V] {
  require(capacity > 0, "capacity must be positive")

  private val values = mutable.LinkedHashMap[Hash, V]()

  override def get(key: Hash): Option[V] = values.get(key)

  override def put(key: Hash, value: V): Unit = {
    values.put(key, value)
    while (values.size > capacity) values.remove(values.head._1)
  }

  override def clear(): Unit = values.clear()
}
