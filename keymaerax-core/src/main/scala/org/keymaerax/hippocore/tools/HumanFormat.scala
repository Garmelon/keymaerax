/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.tools

/** Formatting operations for humans. */
object HumanFormat {
  def pluralize(singular: String, plural: Option[String] = None)(isPlural: Boolean): String = {
    if (!isPlural) return singular
    plural match {
      case Some(plural) => plural
      case None if singular.endsWith("s") => singular + "es"
      case None if singular.endsWith("x") => singular + "es"
      case None => singular + "s"
    }
  }

  def pluralizeN(singular: String, plural: Option[String] = None)(amount: Int): String =
    pluralize(singular, plural)(amount != 1)
}
