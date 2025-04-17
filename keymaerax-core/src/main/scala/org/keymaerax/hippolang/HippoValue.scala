/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.core.Expression
import org.keymaerax.hippocore.proof.{HippoPremise, HippoProof}
import org.keymaerax.hippocore.tools.{Hashable, Hasher, SequentPrinter}
import org.keymaerax.hippolang.namespace.ImmutableNamespace
import org.keymaerax.{core, hippocore}

sealed trait HippoValue extends Hashable {
  def isTruthy: Boolean = true
  def asInt: Int = throw new IllegalArgumentException("value is not an integer")
  def asString: String = throw new IllegalArgumentException("value is not a string")
  def asExpression: core.Expression = throw new IllegalArgumentException("value is not a dL expression")
  def asSequent: core.Sequent = throw new IllegalArgumentException("value is not a dL sequent")
  def asProof: HippoProof = throw new IllegalArgumentException("value is not a proof")
  def asTactic: hippocore.Tactic = throw new IllegalArgumentException("value is not a tactic")

  def format: String
}

object HippoValue {
  final case object Null extends HippoValue {
    override def isTruthy: Boolean = false
    override def format: java.lang.String = "null"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type]
  }

  final case class Bool(value: scala.Boolean) extends HippoValue {
    override def isTruthy: Boolean = value
    override def format: java.lang.String = value.toString
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class Int(value: scala.Int) extends HippoValue {
    override def asInt: scala.Int = value
    override def format: java.lang.String = value.toString
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class String(value: java.lang.String) extends HippoValue {
    override def asString: java.lang.String = value
    override def format: java.lang.String = s"\"${value.replace("\\", "\\\\").replace("\"", "\\\"")}\""
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class List(values: IndexedSeq[HippoValue]) extends HippoValue {
    override def format: java.lang.String = s"list(${values.map(_.format).mkString(", ")})"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digestSeq(values)
  }

  final case class DlExpression(value: core.Expression) extends HippoValue {
    override def asExpression: Expression = value
    override def format: java.lang.String = s"dL{ ${value.prettyString} }"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class DlSequent(value: core.Sequent) extends HippoValue {
    override def asSequent: core.Sequent = value
    override def format: java.lang.String = s"dLs{ ${SequentPrinter.smart(value)} }"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class Namespace(value: ImmutableNamespace) extends HippoValue {
    override def format: java.lang.String = s"<namespace ${value.hash.hexString}>"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class Proof(value: HippoProof) extends HippoValue {
    override def asProof: HippoProof = value
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

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class Tactic(value: hippocore.Tactic) extends HippoValue {
    override def asTactic: hippocore.Tactic = value
    override def format: java.lang.String = s"<tactic ${value.getClass.getName} ${value.hash.hexString}>"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class ProofInfo(value: org.keymaerax.hippolib.meta.ProofInfo) extends HippoValue {
    override def asProof: HippoProof = value.proof
    override def format: java.lang.String = s"<proof info for ${Proof(value.proof).format}>"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class TacticInfo(value: org.keymaerax.hippolib.meta.TacticInfo) extends HippoValue {
    override def asTactic: hippocore.Tactic = value.constructor.constructPositional(IndexedSeq())
    override def format: java.lang.String = s"<tactic info for ${value.constructor.hash}>"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  // Function-like

  final case class BuiltinFunction(value: org.keymaerax.hippolang.BuiltinFunction) extends HippoValue {
    override def format: java.lang.String = s"<builtin function ${value.name}>"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class BuiltinMemberFunction(target: HippoValue, value: org.keymaerax.hippolang.BuiltinMemberFunction)
      extends HippoValue {

    override def format: java.lang.String = s"<builtin member function ${value.name}>"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class Function(env: ImmutableNamespace, args: Seq[HippoIdentifier], body: HippoExpression)
      extends HippoValue {

    override def format: java.lang.String = {
      val hash = Hasher().digest(this).hash
      s"<function ${hash.hexString}>"
    }

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(env).digestSeq(args).digest(body)
  }
}
