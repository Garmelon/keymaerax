/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.parse

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SourceFileTest extends AnyFunSuite with Matchers {
  test("simple file") {
    val file = SourceFile("ab\nc\ndef\n")

    file.line(0).text shouldBe "ab\n"
    file.line(1).text shouldBe "c\n"
    file.line(2).text shouldBe "def\n"
    file.line(3).text shouldBe ""
    an[IllegalArgumentException] should be thrownBy file.line(-1)
    an[IllegalArgumentException] should be thrownBy file.line(4)

    file.rowColAt(0) shouldBe (0, 0)
    file.rowColAt(1) shouldBe (0, 1)
    file.rowColAt(2) shouldBe (0, 2)
    file.rowColAt(3) shouldBe (1, 0)
    file.rowColAt(4) shouldBe (1, 1)
    file.rowColAt(5) shouldBe (2, 0)
    file.rowColAt(6) shouldBe (2, 1)
    file.rowColAt(7) shouldBe (2, 2)
    file.rowColAt(8) shouldBe (2, 3)
    file.rowColAt(9) shouldBe (3, 0)
    an[IllegalArgumentException] should be thrownBy file.rowColAt(-1)
    an[IllegalArgumentException] should be thrownBy file.rowColAt(10)
  }

  test("empty file") {
    val file = SourceFile("")

    file.line(0).text shouldBe ""
    an[IllegalArgumentException] should be thrownBy file.line(-1)
    an[IllegalArgumentException] should be thrownBy file.line(1)

    file.rowColAt(0) shouldBe (0, 0)
    an[IllegalArgumentException] should be thrownBy file.rowColAt(-1)
    an[IllegalArgumentException] should be thrownBy file.rowColAt(1)
  }

  test("file starting with and ending without newline") {
    val file = SourceFile("\nabc")

    file.line(0).text shouldBe "\n"
    file.line(1).text shouldBe "abc"
    an[IllegalArgumentException] should be thrownBy file.line(-1)
    an[IllegalArgumentException] should be thrownBy file.line(2)

    file.rowColAt(0) shouldBe (0, 0)
    file.rowColAt(1) shouldBe (1, 0)
    file.rowColAt(2) shouldBe (1, 1)
    file.rowColAt(3) shouldBe (1, 2)
    file.rowColAt(4) shouldBe (1, 3)
    an[IllegalArgumentException] should be thrownBy file.rowColAt(-1)
    an[IllegalArgumentException] should be thrownBy file.rowColAt(5)
  }

  test("file with surrogate pairs") {
    val file = SourceFile("𐌞\n𐌞")

    file.line(0).text shouldBe "𐌞\n"
    file.line(1).text shouldBe "𐌞"
    an[IllegalArgumentException] should be thrownBy file.line(-1)
    an[IllegalArgumentException] should be thrownBy file.line(2)

    file.rowColAt(0) shouldBe (0, 0)
    file.rowColAt(1) shouldBe (0, 1)
    file.rowColAt(2) shouldBe (0, 2)
    file.rowColAt(3) shouldBe (1, 0)
    file.rowColAt(4) shouldBe (1, 1)
    file.rowColAt(5) shouldBe (1, 2)
    an[IllegalArgumentException] should be thrownBy file.rowColAt(-1)
    an[IllegalArgumentException] should be thrownBy file.rowColAt(6)
  }
}
