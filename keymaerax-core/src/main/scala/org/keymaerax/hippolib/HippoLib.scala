/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib

import org.keymaerax.core.hippolib.publish
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.UniqueNameRegistry
import org.keymaerax.hippolib
import org.keymaerax.hippolib.meta.{TacticArg, TacticArgInfo, TacticInfo}

class HippoLib(implicit ctx: HippoContext) {
  implicit private val lib: HippoLib = this
  implicit private val names: UniqueNameRegistry = new UniqueNameRegistry

  @publish
  val core: hippolib.core.Lib = new hippolib.core.Lib

  @publish
  val belle: hippolib.belle.Lib = new hippolib.belle.Lib

  @publish(name = "Belle")
  val Belle: TacticInfo = TacticInfo.arg2(
    "Belle",
    TacticArgInfo(name = "name", arg = TacticArg.String),
    TacticArgInfo(name = "args", arg = TacticArg.Seq(TacticArg.BelleValue)),
    vararg = true,
  ) { (name, args) => org.keymaerax.hippolib.belle.Belle(name, args) }

  val db: HippoLibDb = HippoLibDb.empty.addPublished(this)
}
