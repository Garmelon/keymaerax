/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.hippolang.parse.HippoParser

case class HippoIdentifier(value: String) {
  require(HippoIdentifier.isValid(value), "invalid identifier")

  def plain: String = value
  def quoted: String = s"'$value'"
  def quotedIfNecessary: String = if (HippoParser.keywords.contains(value)) quoted else plain

  override def toString: String = quotedIfNecessary
}

object HippoIdentifier {
  private def isAsciiLetter(c: Int): Boolean = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
  private def isAsciiDigit(c: Int): Boolean = '0' <= c && c <= '9'

  def isValidStartChar(c: Int): Boolean = isAsciiLetter(c) || c == '_'
  def isValidChar(c: Int): Boolean = isAsciiLetter(c) || isAsciiDigit(c) || c == '_'

  def isValid(str: String): Boolean = {
    // An empty identifier can't be distinguished from "no identifier" by a parser.
    // Identifiers must not start with a digit, or parsers could confuse them with integers.
    val validStartChar = str.codePoints().limit(1).allMatch(isValidStartChar)
    val validRestChars = str.codePoints().skip(1).allMatch(isValidChar)
    str.nonEmpty && validStartChar && validRestChars
  }
}
