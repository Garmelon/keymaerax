/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippolang.HippoConversions._
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace, Namespace}
import org.keymaerax.hippolang.{BuiltinFunction, HippoIdentifier, HippoValue}
import org.keymaerax.hippolib.HippoLib

import scala.collection.mutable

class EnvBuilder(child: Option[Namespace] = None) {
  private val env = mutable.Map.empty[HippoIdentifier, EnvBuilder.Variable]

  def add(name: HippoIdentifier, value: HippoValue, mutable: Boolean = false): EnvBuilder = {
    env.put(name, EnvBuilder.VarValue(value, mutable))
    this
  }

  def addPath(path: List[HippoIdentifier], value: HippoValue, mutable: Boolean = false): EnvBuilder = {
    path match {
      case Nil => throw new IllegalArgumentException("path must not be empty")
      case name :: Nil => add(name, value, mutable)
      case name :: rest => env.get(name) match {
          case Some(v: EnvBuilder.VarBuilder) => v.builder.addPath(rest, value, mutable)
          case Some(_: EnvBuilder.VarValue) => throw new IllegalArgumentException("path must point to valid location")
          case None =>
            val builder = new EnvBuilder().addPath(rest, value, mutable)
            env.put(name, EnvBuilder.VarBuilder(builder, mutable = false))
        }
    }
    this
  }

  def addBuiltins(): EnvBuilder = {
    add(BuiltinFunction.List.name, BuiltinFunction.List.toHValue)
    add(BuiltinFunction.Print.name, BuiltinFunction.Print.toHValue)
    add(BuiltinFunction.Premise.name, BuiltinFunction.Premise.toHValue)

    for (builtin <- BuiltinFunction.all) addPath(List(HippoIdentifier("builtins"), builtin.name), builtin.toHValue)

    this
  }

  def addHippoLib(lib: HippoLib): EnvBuilder = {
    val byName = mutable.Map.empty[String, HippoValue]
    for ((name, info) <- lib.db.proofs) byName.put(name, info.toHValue)
    for ((name, info) <- lib.db.tactics) byName.put(name, info.toHValue)
    for ((from, to) <- lib.db.aliases) byName.put(from, byName(to))

    for ((name, value) <- byName) {
      val path = name.split('.').map(HippoIdentifier(_)).toList
      addPath(path, value)
    }

    this
  }

  def build(): ImmutableNamespace = {
    val ns = new MutableNamespace(child)

    for ((name, variable) <- env) variable match {
      case EnvBuilder.VarValue(value, mutable) => ns.declare(name, value, mutable)
      case EnvBuilder.VarBuilder(builder, mutable) => ns.declare(name, builder.build().toHValue, mutable)
    }

    ns.freeze
  }
}

object EnvBuilder {
  private sealed trait Variable
  private case class VarValue(value: HippoValue, mutable: Boolean) extends Variable
  private case class VarBuilder(builder: EnvBuilder, mutable: Boolean) extends Variable
}
