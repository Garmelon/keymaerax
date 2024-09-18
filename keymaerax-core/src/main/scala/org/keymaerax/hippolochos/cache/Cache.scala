/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.cache

import org.keymaerax.hippolochos.tools.Hash

trait Cache[V] {
  def get(key: Hash): Option[V]

  def put(key: Hash, value: V): Unit

  def clear(): Unit

  def getOrCompute(key: Hash)(value: => V): V = get(key).getOrElse {
    val computed = value
    put(key, computed)
    computed
  }

  final def behind(other: Cache[V]): ChainCache[V] = new ChainCache(other, this)
}
