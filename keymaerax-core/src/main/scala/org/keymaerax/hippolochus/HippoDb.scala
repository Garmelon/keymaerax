/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus

import io.github.classgraph.{ClassGraph, ClassInfo, ScanResult}
import org.keymaerax.core.hippolochus.annotations.{Axiom, Rule, Tactic}
import org.keymaerax.hippolochus.axiom.HippoAxiom
import org.keymaerax.hippolochus.rule.HippoRule
import org.keymaerax.hippolochus.tactic.HippoTactic

import java.lang.annotation.Annotation

case class HippoDb(
    axioms: Map[HippoIdentifier, HippoAxiom],
    rules: Map[HippoIdentifier, HippoRule],
    tactics: Map[HippoIdentifier, HippoTactic],
)

object HippoDb {
  private def getAnnotatedFieldsOfClass[A <: Annotation, T](
      info: ClassInfo,
      annotation: Class[A],
      getName: A => String,
      getValue: AnyRef => T,
  ): Seq[(HippoIdentifier, T)] = {
    require(info.getName.endsWith("$"), "annotations in normal classes not allowed")
    val clazz = info.loadClass()
    val instance = clazz.getField("MODULE$").get(null)

    clazz
      .getDeclaredFields
      .filter(_.isAnnotationPresent(annotation))
      .map { field =>
        val method = clazz.getMethod(field.getName)
        val name = HippoIdentifier(getName(field.getAnnotation(annotation)))
        val value = getValue(method.invoke(instance))
        name -> value
      }
  }

  private def getAllAnnotatedFields[A <: Annotation, T](
      scan: ScanResult,
      annotation: Class[A],
      getName: A => String,
      getValue: AnyRef => T,
  ): Seq[(HippoIdentifier, T)] = {
    import scala.jdk.CollectionConverters._
    scan
      .getClassesWithFieldAnnotation(annotation)
      .asScala
      .toSeq
      .flatMap(info => getAnnotatedFieldsOfClass(info, annotation, getName, getValue))
  }

  def fromAnnotationScan(): HippoDb = {
    val scan = new ClassGraph().enableAllInfo().scan()
    val axioms = getAllAnnotatedFields[Axiom, HippoAxiom](scan, classOf[Axiom], _.name(), _.asInstanceOf[HippoAxiom])
    val rules = getAllAnnotatedFields[Rule, HippoRule](scan, classOf[Rule], _.name(), _.asInstanceOf[HippoRule])
    val tactics =
      getAllAnnotatedFields[Tactic, HippoTactic](scan, classOf[Tactic], _.name(), _.asInstanceOf[HippoTactic])
    require(axioms.map(_._1).distinct.length == axioms.length, "axiom names must be unique")
    require(rules.map(_._1).distinct.length == rules.length, "rule names must be unique")
    require(tactics.map(_._1).distinct.length == tactics.length, "axiom names must be unique")
    HippoDb(axioms = axioms.toMap, rules = rules.toMap, tactics = tactics.toMap)
  }
}
