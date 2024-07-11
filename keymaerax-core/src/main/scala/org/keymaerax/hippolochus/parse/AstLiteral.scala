/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.parse

import org.keymaerax.core
import org.keymaerax.hippolochus.{HippoBuiltin, HippoValue}
import org.keymaerax.parser.KeYmaeraXPrettyPrinter

sealed trait AstLiteral
object AstLiteral {
  case class Null() extends AstLiteral {
    override def toString: java.lang.String = "null"
  }

  case class Bool(value: scala.Boolean) extends AstLiteral {
    override def toString: java.lang.String = value.toString
  }

  case class Int(value: scala.Int) extends AstLiteral {
    override def toString: java.lang.String = value.toString
  }

  case class String(value: java.lang.String) extends AstLiteral {
    override def toString: java.lang.String = {
      val escaped = value.flatMap {
        case '\\' => "\\\\"
        case '"' => "\\\""
        case c => Character.toString(c)
      }
      s"\"${escaped.mkString}\""
    }
  }

  case class Dgl(value: core.Formula) extends AstLiteral {
    override def toString: java.lang.String = s"dGL{ ${KeYmaeraXPrettyPrinter(value)} }"
  }

  case class Builtin(value: HippoBuiltin) extends AstLiteral {
    override def toString: java.lang.String = s"#${value.name}"
  }

  def toHippoValue(literal: AstLiteral): HippoValue = literal match {
    case Null() => HippoValue.Null
    case Bool(value) => HippoValue.Bool(value)
    case Int(value) => HippoValue.Int(value)
    case String(value) => HippoValue.String(value)
    case Dgl(value) => HippoValue.Dgl(value)
    case Builtin(value) => HippoValue.Builtin(value)
  }
}
