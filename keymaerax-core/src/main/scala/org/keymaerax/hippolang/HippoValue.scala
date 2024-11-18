/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.core.Sequent
import org.keymaerax.hippolang.namespace.ImmutableNamespace
import org.keymaerax.hippolochos.proof.{HippoPremise, HippoProof}
import org.keymaerax.hippolochos.tools.{Hash, SequentPrinter}
import org.keymaerax.{core, hippolochos}

sealed trait HippoValue {
  def isTruthy: Boolean = true
  def asInt: Int = throw new IllegalArgumentException("value is not an integer")
  def asString: String = throw new IllegalArgumentException("value is not a string")
  def asSequent: Sequent = throw new IllegalArgumentException("value is not a dL sequent")
  def asTactic: hippolochos.Tactic = throw new IllegalArgumentException("value is not a tactic")

  def format: String
}

object HippoValue {
  final case object Null extends HippoValue {
    override def isTruthy: Boolean = false
    override def format: java.lang.String = "null"
  }

  final case class Bool(value: scala.Boolean) extends HippoValue {
    override def isTruthy: Boolean = value
    override def format: java.lang.String = value.toString
  }

  final case class Int(value: scala.Int) extends HippoValue {
    override def asInt: scala.Int = value
    override def format: java.lang.String = value.toString
  }

  final case class String(value: java.lang.String) extends HippoValue {
    override def asString: java.lang.String = value
    override def format: java.lang.String = s"\"${value.replace("\\", "\\\\").replace("\"", "\\\"")}\""
  }

  final case class List(values: IndexedSeq[HippoValue]) extends HippoValue {
    override def format: java.lang.String = s"list(${values.map(_.format).mkString(", ")})"
  }

  final case class DlExpression(value: core.Expression) extends HippoValue {
    override def format: java.lang.String = s"dL { ${value.prettyString} }"
  }

  final case class DlSequent(value: core.Sequent) extends HippoValue {
    override def asSequent: core.Sequent = value
    override def format: java.lang.String = s"dLs { ${SequentPrinter.smart(value)} }"
  }

  final case class Namespace(value: ImmutableNamespace) extends HippoValue {
    override def format: java.lang.String = s"<namespace ${value.hash.hexString}>"
  }

  final case class Proof(value: HippoProof) extends HippoValue {
    override def format: java.lang.String = {
      if (value.premises.isEmpty) return s"<proof of ${SequentPrinter.oneline(value.conclusion)}>"
      val premises = value
        .premises
        .map {
          case HippoPremise(sequent, false) => s"\n  given  ${SequentPrinter.oneline(sequent)}"
          case HippoPremise(sequent, true) => s"\n  given  ${SequentPrinter.oneline(sequent)} (must be proved)"
        }
      s"<proof\n  of     ${SequentPrinter.oneline(value.conclusion)}${premises.mkString}\n>"
    }
  }

  final case class Tactic(value: hippolochos.Tactic) extends HippoValue {
    override def asTactic: hippolochos.Tactic = value
    override def format: java.lang.String = s"<tactic ${value.getClass.getName} ${value.hash.hexString}>"
  }

  final case class ProofInfo(value: org.keymaerax.hippolib.meta.ProofInfo) extends HippoValue {
    override def format: java.lang.String = s"<proof info for ${Proof(value.proof).format}>"
  }

  final case class TacticInfo(value: org.keymaerax.hippolib.meta.TacticInfo) extends HippoValue {
    override def format: java.lang.String = s"<tactic info for ${value.constructor.hash}>"
  }

  // Function-like

  final case class BuiltinFunction(value: org.keymaerax.hippolang.BuiltinFunction) extends HippoValue {
    override def format: java.lang.String = s"<builtin function ${value.name}>"
  }

  final case class BuiltinMemberFunction(target: HippoValue, value: org.keymaerax.hippolang.BuiltinMemberFunction)
      extends HippoValue {

    override def format: java.lang.String = s"<builtin member function ${value.name}>"
  }

  final case class Function(env: ImmutableNamespace, args: Seq[HippoIdentifier], body: HippoExpression)
      extends HippoValue {

    override def format: java.lang.String = {
      val hash = Hash.start.digest(this).build
      s"<function ${hash.hexString}>"
    }
  }
}
