/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib

import org.keymaerax.core.hippolib.publish
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.UniqueNameRegistry
import org.keymaerax.hippolib

class HippoLib(implicit ctx: HippoContext) {
  implicit private val lib: HippoLib = this
  implicit private val names: UniqueNameRegistry = new UniqueNameRegistry

  @publish
  val core: hippolib.core.Lib = new hippolib.core.Lib

  @publish
  val belle: hippolib.belle.Lib = new hippolib.belle.Lib

  val db: HippoLibDb = HippoLibDb.empty.addPublished(this)
}
