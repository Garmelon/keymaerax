/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.namespace

import org.keymaerax.hippolang.{HlangIdentifier, HlangValue}

import scala.collection.mutable

final class MutableNamespace(private val child: Option[Namespace] = None) extends Namespace {
  private val variables: mutable.Map[HlangIdentifier, MutableNamespace.Variable] = mutable.Map.empty

  override def declare(name: HlangIdentifier, value: HlangValue, mutable: Boolean): Unit = {
    require(!variables.contains(name), s"variable $name already declared in current scope")
    variables.put(name, new MutableNamespace.Variable(value, mutable))
  }

  override def assign(name: HlangIdentifier, value: HlangValue): Unit = variables.get(name) match {
    case Some(variable) if variable.mutable => variable.value = value
    case Some(_) => throw new IllegalArgumentException(s"variable $name is immutable")
    case None =>
      child.getOrElse(throw new IllegalArgumentException(s"variable $name does not exist")).assign(name, value)
  }

  override def lookup(name: HlangIdentifier): HlangValue = lookupOpt(name)
    .getOrElse(throw new IllegalArgumentException(s"variable $name does not exist"))

  override def lookupOpt(name: HlangIdentifier): Option[HlangValue] = variables
    .get(name)
    .map(_.value)
    .orElse(child.flatMap(_.lookupOpt(name)))

  override def freeze: ImmutableNamespace =
    ImmutableNamespace(variables = variables.view.mapValues(_.value).toMap, child = child.map(_.freeze))
}

private object MutableNamespace {
  private class Variable(var value: HlangValue, val mutable: Boolean)
}
