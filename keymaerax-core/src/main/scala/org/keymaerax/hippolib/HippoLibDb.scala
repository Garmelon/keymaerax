/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib

import org.keymaerax.core.hippolib.publish
import org.keymaerax.hippolib.meta.{ProofInfo, TacticInfo}

case class HippoLibDb(
    proofs: Map[String, ProofInfo] = Map.empty,
    tactics: Map[String, TacticInfo] = Map.empty,
    aliases: Map[String, String] = Map.empty,
) {
  def isNameKnown(name: String): Boolean = proofs.contains(name) || tactics.contains(name) || aliases.contains(name)

  def addAlias(from: String, to: String): HippoLibDb = {
    require(!isNameKnown(from))
    require(isNameKnown(to))
    copy(aliases = aliases.updated(from, to))
  }

  def addProof(name: String, proof: ProofInfo): HippoLibDb = {
    require(!isNameKnown(name))
    copy(proofs = proofs.updated(name, proof))
  }

  def addTactic(name: String, tactic: TacticInfo): HippoLibDb = {
    require(!isNameKnown(name))
    copy(tactics = tactics.updated(name, tactic))
  }

  def addPublished(obj: Object): HippoLibDb = {
    val clazz = obj.getClass
    var result = this

    for (field <- clazz.getDeclaredFields.toSeq.filter(_.isAnnotationPresent(classOf[publish]))) {
      val annotation = field.getAnnotation(classOf[publish])
      val name = annotation.name()
      val aliases = annotation.aliases()

      // Val fields are private but have public getter functions of the same name.
      // TODO Fail gracefully if invoking fails
      val value = clazz.getMethod(field.getName).invoke(obj)

      if (name == "") {
        require(aliases.isEmpty)
        result = result.addPublished(value)
      } else {
        value match {
          case v: ProofInfo => result = result.addProof(name, v)
          case v: TacticInfo => result = result.addTactic(name, v)
          case _ =>
            throw new Exception(s"Only ${classOf[ProofInfo]} and ${classOf[TacticInfo]} can be published under a name.")
        }
        for (alias <- aliases) result = result.addAlias(alias, name)
      }
    }

    result
  }
}

object HippoLibDb {
  def empty: HippoLibDb = HippoLibDb()
}
