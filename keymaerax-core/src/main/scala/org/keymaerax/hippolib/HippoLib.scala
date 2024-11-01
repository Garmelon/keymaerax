/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib

import org.keymaerax.core.hippolib.publish
import org.keymaerax.hippolib
import org.keymaerax.hippolochos.run.HippoContext

class HippoLib(implicit ctx: HippoContext) {
  implicit private val lib: HippoLib = this

  @publish
  val core: hippolib.core.Lib = new hippolib.core.Lib

  val db: HippoLibDb = HippoLibDb.empty.addPublished(this)
}
