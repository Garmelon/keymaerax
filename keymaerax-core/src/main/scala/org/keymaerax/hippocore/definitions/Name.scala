/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.definitions

import org.keymaerax.core.{DotTerm, NamedSymbol, Nothing}
import org.keymaerax.hippocore.tools.{Hashable, Hasher}

import scala.math.Ordered.orderingToOrdered

case class Name(name: String, index: Option[Int] = None) extends Ordered[Name] with Hashable {
  // Naming convention checks that mirror the checks done in the core.
  if (name != DotTerm().name && name != Nothing.name) {
    val firstUnderscore = name.indexOf('_')
    val (chars, underscores) = if (firstUnderscore >= 0) name.splitAt(firstUnderscore) else (name, "")
    require(chars.nonEmpty, "name must not be empty")
    require(chars.head.isLetter, "name must start with a letter")
    require(chars.forall(_.isLetterOrDigit), "name must only contain letters and digits")
    require(underscores.forall(_ == '_'), "underscores may only appear at the end of a name")
  }
  require(index.forall(_ >= 0), "index must not be negative")

  override def compare(that: Name): Int = (name, index) compare (that.name, that.index)

  override def digestInto(hasher: Hasher): Unit = hasher.digest(name).digestOptWith(index)(_.digest(_))

  override def toString: String = index match {
    case Some(index) => s"${name}_$index"
    case None => name
  }
}

object Name {
  def apply(ns: NamedSymbol): Name = Name(name = ns.name, index = ns.index)

  def parse(s: String): Name = {
    val lastUnderscore = s.lastIndexOf('_')
    if (lastUnderscore < 0) return Name(name = s) // No underscore
    if (lastUnderscore == s.length - 1) return Name(name = s) // Ends with underscore
    val name = s.slice(0, lastUnderscore)
    val index = s.slice(lastUnderscore + 1, s.length).toInt
    Name(name = name, index = Some(index))
  }
}
