/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib

import org.keymaerax.btactics.macros.DerivationInfo
import org.keymaerax.core.hippolib.publish
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippolib
import org.keymaerax.hippolib.HippoLib.addBelleDerivationInfos
import org.keymaerax.hippolib.meta.{TacticArg, TacticArgInfo, TacticInfo}

class HippoLib(implicit ctx: HippoContext) {
  implicit private val lib: HippoLib = this

  @publish
  val core: hippolib.core.Lib = new hippolib.core.Lib

  // This tactic is not in the "belle" namespace because it would be annoying to have to type `belle.Belle("name")`.
  // If hippolang ever gains the ability to import individual items to the current namespace,
  // it might make sense to move it to the "belle" namespace.
  @publish
  val Belle: TacticInfo = TacticInfo.arg2(
    "Belle",
    TacticArgInfo(name = "name", arg = TacticArg.String),
    TacticArgInfo(name = "args", arg = TacticArg.Seq(TacticArg.BelleValue)),
    vararg = true,
  ) { (name, args) => org.keymaerax.hippolib.belle.Belle(name, args) }

  val db: HippoLibDb = addBelleDerivationInfos(HippoLibDb.empty.addPublished(this))
}

object HippoLib {

  /** Add all of Bellerophon's [[DerivationInfo]]s as tactics to the `belle` namespace. */
  private def addBelleDerivationInfos(db: HippoLibDb): HippoLibDb = DerivationInfo
    .allInfo
    .values
    .foldLeft(db) { (db, info) =>
      db.addTactic(
        TacticInfo.arg1(
          s"belle.${info.codeName}",
          TacticArgInfo(name = "args", arg = TacticArg.Seq(TacticArg.BelleValue)),
          vararg = true,
        ) { args => org.keymaerax.hippolib.belle.Belle(info.codeName, args) }
      )
    }
}
