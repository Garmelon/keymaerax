/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus

import fastparse.Parsed
import org.keymaerax.hippolochus.parse.HippoParser
import org.keymaerax.{Configuration, FileConfiguration}

object Test {
  val src: String = """
                      |print("Hello \"world\"!");
                      |myFormula = dGL { x=1 -> [x:=x+1;] x=2 };
                      |additionWorks = theorem dGL { 1+1=2 } by {
                      |  auto();
                      |};
                      |on "hey" { done; };
                      |fork {
                      |  on 1 { done; };
                      |  on 2 { done; };
                      |};
                      |""".stripMargin

  def main(args: Array[String]): Unit = {
    Configuration.setConfiguration(FileConfiguration)
    val parsed = HippoParser.parse(src)

    val expression = parsed match {
      case Parsed.Success(value, _) => value
      case f: Parsed.Failure =>
        println(f.trace().terminalsMsg)
        println(f.trace().aggregateMsg)
        println(f.trace().longTerminalsMsg)
        println(f.trace().longAggregateMsg)
        return
    }

    println(expression)
  }
}
