/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.cache

import org.keymaerax.hippolochos.proof.{HippoJson, HippoProof}
import spray.json._

import java.nio.file.Path

class HippoProofFsCache(dir: Path) extends FsCache[HippoProof](dir, version = 0) {
  override def format(value: HippoProof): String = HippoJson.proofsToJson(Seq(value)).compactPrint

  override def parse(str: String): HippoProof = {
    val Seq(proof) = HippoJson.jsonToProofs(str.parseJson)
    proof
  }
}
