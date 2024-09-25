/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib

import io.github.classgraph.ClassGraph
import org.keymaerax.core.hippolib.publish
import org.keymaerax.hippolib.meta.{ProofInfo, TacticInfo}

import java.lang.reflect.Field

case class HippoLibDb(proofs: Map[String, ProofInfo], tactics: Map[String, TacticInfo], aliases: Map[String, String]) {
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

  def addTactic(name: String, proof: TacticInfo): HippoLibDb = {
    require(!isNameKnown(name))
    copy(tactics = tactics.updated(name, proof))
  }
}

object HippoLibDb {
  def empty: HippoLibDb = HippoLibDb(proofs = Map.empty, tactics = Map.empty, aliases = Map.empty)

  private def getObjectInstance(clazz: Class[_]): AnyRef = {
    // An object's instance can be located through its public static final MODULE$ field.
    // TODO Fail gracefully if this is not a Scala object
    clazz.getField("MODULE$").get(null)
  }

  private def getObjectField(clazz: Class[_], instance: AnyRef, field: Field): AnyRef = {
    // Val fields are private but have public getter functions of the same name.
    val getter = clazz.getMethod(field.getName)

    getter.invoke(instance)
  }

  private def findPublishedThings(): Seq[(String, publish, AnyRef)] = {
    import scala.jdk.CollectionConverters._

    val publishingObjects = new ClassGraph()
      .enableAllInfo()
      .scan()
      .getClassesWithFieldAnnotation(classOf[publish])
      .asScala
      .toSeq

    publishingObjects.flatMap { classInfo =>
      val clazz = classInfo.loadClass()
      val instance = getObjectInstance(clazz)

      clazz
        .getDeclaredFields
        .toSeq
        .filter(_.isAnnotationPresent(classOf[publish]))
        .map { field =>
          val path = s"${field.getName} in ${clazz.getName}"
          val annotation = field.getAnnotation(classOf[publish])
          val thing = getObjectField(clazz, instance, field)
          (path, annotation, thing)
        }
    }
  }

  def published(): HippoLibDb = {
    var found = HippoLibDb.empty

    for ((path, annotation, thing) <- findPublishedThings()) {
      val name = annotation.name()
      val aliases = annotation.aliases()

      thing match {
        case t: ProofInfo => found = found.addProof(name, t)
        case t: TacticInfo => found = found.addTactic(name, t)
        case _ => throw new Exception(
            s"published field $path is neither ${classOf[ProofInfo].getName} nor ${classOf[TacticInfo].getName}"
          )
      }

      for (alias <- aliases) found = found.addAlias(alias, name)
    }

    found
  }
}
