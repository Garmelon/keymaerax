/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.interpret

import org.keymaerax.hippolochus.{HippoBuiltin, HippoDb, HippoIdentifier, HippoValue}

case class Namespace(values: Map[HippoIdentifier, HippoValue] = Map(), fallback: Option[Namespace] = None) {
  def lookupOption(name: HippoIdentifier): Option[HippoValue] = values
    .get(name)
    .orElse(fallback.flatMap(_.lookupOption(name)))

  def lookup(name: HippoIdentifier): HippoValue = lookupOption(name)
    .getOrElse(throw new NameNotFoundException(this, name))

  def assign(name: HippoIdentifier, value: HippoValue): Namespace = {
    if (lookupOption(name).nonEmpty) throw new NameAlreadyDefinedException(this, name)
    copy(values = values.updated(name, value))
  }

  def nest(): Namespace = Namespace(fallback = Some(this))
}

object Namespace {
  def builtins: Namespace = { Namespace(values = HippoBuiltin.all.map(b => b.name -> HippoValue.Builtin(b)).toMap) }

  def populated(db: HippoDb): Namespace = {
    val axioms = db.axioms.view.mapValues(HippoValue.Axiom).toMap
    val rules = db.rules.view.mapValues(HippoValue.Rule).toMap
    val tactics = db.tactics.view.mapValues(HippoValue.Tactic).toMap

    Namespace(
      values = Map(
        HippoIdentifier("a") -> HippoValue.Namespace(axioms),
        HippoIdentifier("r") -> HippoValue.Namespace(rules),
        HippoIdentifier("t") -> HippoValue.Namespace(tactics),
      ),
      fallback = Some(builtins),
    )
  }
}
