/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.belle

import org.keymaerax.core.SeqPos
import org.keymaerax.core.hippolib.publish
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{ExprPath, UniqueNameRegistry}
import org.keymaerax.hippolib.HippoLib
import org.keymaerax.hippolib.meta.{TacticArg, TacticArgInfo, TacticInfo}

/** A collection of tactics ported from [[org.keymaerax.bellerophon]]. */
class Lib(implicit ctx: HippoContext, lib: HippoLib, names: UniqueNameRegistry) {
  @publish(name = "belle.useAt")
  val useAt: TacticInfo = TacticInfo(
    "belle.useAt",
    TacticArgInfo(name = "proof", arg = TacticArg.ProofInfo),
    TacticArgInfo(name = "sequent", arg = TacticArg.SeqPos, default = Some(SeqPos(1))),
    TacticArgInfo(name = "path", arg = TacticArg.ExprPath, default = Some(ExprPath())),
    TacticArgInfo(name = "key", arg = TacticArg.ExprPath, default = Some(ExprPath())),
  ) { (proof, sequent, path, key) => UseAt(proof, sequent, path, key) }
}
