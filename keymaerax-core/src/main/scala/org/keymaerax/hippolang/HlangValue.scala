/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.hippocore
import org.keymaerax.hippocore.proof.{HippoExpression, HippoPremise, HippoProof, HippoSequent}
import org.keymaerax.hippocore.tools.{Hashable, Hasher, SequentPrinter}
import org.keymaerax.hippolang.namespace.ImmutableNamespace

sealed trait HlangValue extends Hashable {
  def isTruthy: Boolean = true
  def asInt: Int = throw new IllegalArgumentException("value is not an integer")
  def asString: String = throw new IllegalArgumentException("value is not a string")
  def asExpression: HippoExpression = throw new IllegalArgumentException("value is not a dL expression")
  def asSequent: HippoSequent = throw new IllegalArgumentException("value is not a dL sequent")
  def asProof: HippoProof = throw new IllegalArgumentException("value is not a proof")
  def asTactic: hippocore.Tactic = throw new IllegalArgumentException("value is not a tactic")

  def format: String
}

object HlangValue {
  final case object Null extends HlangValue {
    override def isTruthy: Boolean = false
    override def format: java.lang.String = "null"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type]
  }

  final case class Bool(value: scala.Boolean) extends HlangValue {
    override def isTruthy: Boolean = value
    override def format: java.lang.String = value.toString
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class Int(value: scala.Int) extends HlangValue {
    override def asInt: scala.Int = value
    override def format: java.lang.String = value.toString
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class String(value: java.lang.String) extends HlangValue {
    override def asString: java.lang.String = value
    override def format: java.lang.String = s"\"${value.replace("\\", "\\\\").replace("\"", "\\\"")}\""
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class List(values: IndexedSeq[HlangValue]) extends HlangValue {
    override def format: java.lang.String = s"list(${values.map(_.format).mkString(", ")})"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digestSeq(values)
  }

  final case class DlExpression(value: HippoExpression) extends HlangValue {
    override def asExpression: HippoExpression = value
    override def format: java.lang.String = s"dL{ ${value.expr.prettyString} }"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class DlSequent(value: HippoSequent) extends HlangValue {
    override def asSequent: HippoSequent = value
    override def format: java.lang.String = s"dLs{ ${SequentPrinter.smart(value.sequent)} }"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class Namespace(value: ImmutableNamespace) extends HlangValue {
    override def format: java.lang.String = s"<namespace ${value.hash.hexString}>"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class Proof(value: HippoProof) extends HlangValue {
    override def asProof: HippoProof = value
    override def format: java.lang.String = {
      if (value.premises.isEmpty) return s"<proof of\n${SequentPrinter.oneline(value.conclusion.sequent)}>"
      val premises = value
        .premises
        .map {
          case HippoPremise(sequent, false) => s"\n  given  ${SequentPrinter.oneline(sequent.sequent)}"
          case HippoPremise(sequent, true) => s"\n  given  ${SequentPrinter.oneline(sequent.sequent)} (must be proved)"
        }
      s"<proof\n  of     ${SequentPrinter.oneline(value.conclusion.sequent)}${premises.mkString}\n>"
    }

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class Tactic(value: hippocore.Tactic) extends HlangValue {
    override def asTactic: hippocore.Tactic = value
    override def format: java.lang.String = s"<tactic ${value.getClass.getName} ${value.hash.hexString}>"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class ProofInfo(value: org.keymaerax.hippolib.meta.ProofInfo) extends HlangValue {
    override def asProof: HippoProof = value.proof
    override def format: java.lang.String = s"<proof info for ${Proof(value.proof).format}>"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class TacticInfo(value: org.keymaerax.hippolib.meta.TacticInfo) extends HlangValue {
    override def asTactic: hippocore.Tactic = value.constructor.constructPositional(IndexedSeq())
    override def format: java.lang.String = s"<tactic info for ${value.constructor.hash}>"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  // Function-like

  final case class BuiltinFunction(value: org.keymaerax.hippolang.BuiltinFunction) extends HlangValue {
    override def format: java.lang.String = s"<builtin function ${value.name}>"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class BuiltinMemberFunction(target: HlangValue, value: org.keymaerax.hippolang.BuiltinMemberFunction)
      extends HlangValue {

    override def format: java.lang.String = s"<builtin member function ${value.name}>"
    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(value)
  }

  final case class Function(env: ImmutableNamespace, args: Seq[HlangIdentifier], body: HlangExpression)
      extends HlangValue {

    override def format: java.lang.String = {
      val hash = Hasher().digest(this).hash
      s"<function ${hash.hexString}>"
    }

    override def digestInto(hasher: Hasher): Unit = hasher.digest[this.type].digest(env).digestSeq(args).digest(body)
  }
}
