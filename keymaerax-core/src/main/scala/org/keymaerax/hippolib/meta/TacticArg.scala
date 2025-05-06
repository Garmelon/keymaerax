/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

import org.keymaerax.hippocore.definitions.Definitions
import org.keymaerax.hippocore.proof.HippoExpression

sealed trait TacticArg {
  type Type
  def name: String
  protected def validateImpl(any: Any): Type

  final def validate(any: Any): Type =
    try validateImpl(any)
    catch { case e: Throwable => throw new IllegalArgumentException(s"expected $name", e) }
}

object TacticArg {
  /////////////////
  // Scala types //
  /////////////////

  case object Int extends TacticArg {
    override type Type = scala.Int
    override def name: String = "Int"
    override def validateImpl(any: Any): Type = any.asInstanceOf[Type]
  }

  case object String extends TacticArg {
    override type Type = java.lang.String
    override def name: String = "String"
    override def validateImpl(any: Any): Type = any.asInstanceOf[Type]
  }

  case class Option[A <: TacticArg](inner: A) extends TacticArg {
    override type Type = scala.Option[inner.Type]
    override def name: String = s"Option[${inner.name}]"
    override def validateImpl(any: Any): Type = any match {
      case v: scala.Option[_] => v.map(inner.validate)
      case v => Some(inner.validate(v))
    }
  }

  case class Seq[A <: TacticArg](inner: A) extends TacticArg {
    override type Type = scala.Seq[inner.Type]
    override def name: String = s"Seq[${inner.name}]"
    override def validateImpl(any: Any): Type = any.asInstanceOf[scala.Seq[?]].map(inner.validate)
  }

  case class Tuple2[A1 <: TacticArg, A2 <: TacticArg](inner1: A1, inner2: A2) extends TacticArg {
    override type Type = (inner1.Type, inner2.Type)
    override def name: String = s"(${inner1.name}, ${inner2.name})"
    override def validateImpl(any: Any): Type = any match {
      case (v1, v2) => (inner1.validate(v1), inner2.validate(v2))
      case scala.Seq(v1, v2) => (inner1.validate(v1), inner2.validate(v2))
    }
  }

  ////////////////
  // Core types //
  ////////////////

  case object SeqPos extends TacticArg {
    override type Type = org.keymaerax.core.SeqPos
    override def name: String = s"SeqPos"
    override def validateImpl(any: Any): Type = any match {
      case v: Type => v
      case v: Int =>
        require(v != 0, "SeqPos must not be 0")
        org.keymaerax.core.SeqPos.apply(v)
    }
  }

  case object AntePos extends TacticArg {
    override type Type = org.keymaerax.core.AntePos
    override def name: String = s"AntePos"
    override def validateImpl(any: Any): Type = any match {
      case v: Type => v
      case v: Int =>
        // TODO Add AntePos constructor to core?
        require(v < 0, "AntePos must be smaller than 0")
        org.keymaerax.core.SeqPos.apply(v).asInstanceOf[Type]
    }
  }

  case object SuccPos extends TacticArg {
    override type Type = org.keymaerax.core.SuccPos
    override def name: String = s"SuccPos"
    override def validateImpl(any: Any): Type = any match {
      case v: Type => v
      case v: Int =>
        // TODO Add SuccPos constructor to core?
        require(v > 0, "SuccPos must be bigger than 0")
        org.keymaerax.core.SeqPos.apply(v).asInstanceOf[Type]
    }
  }

  case object Formula extends TacticArg {
    override type Type = org.keymaerax.core.Formula
    override def name: String = s"Formula"
    override def validateImpl(any: Any): Type = any.asInstanceOf[Type]
  }

  case object Expression extends TacticArg {
    override type Type = org.keymaerax.core.Expression
    override def name: String = s"Expression"
    override def validateImpl(any: Any): Type = any match {
      case v: Type => v
      case v: Int => org.keymaerax.core.Number(v)
    }
  }

  case object Sequent extends TacticArg {
    override type Type = org.keymaerax.core.Sequent
    override def name: String = s"Sequent"
    override def validateImpl(any: Any): Type = any.asInstanceOf[Type]
  }

  case object Variable extends TacticArg {
    override type Type = org.keymaerax.core.Variable
    override def name: String = s"Variable"
    override def validateImpl(any: Any): Type = any.asInstanceOf[Type]
  }

  /////////////////
  // Hippo types //
  /////////////////

  case object ExprPath extends TacticArg {
    override type Type = org.keymaerax.hippocore.tools.ExprPath
    override def name: String = s"ExprPath"
    override def validateImpl(any: Any): Type = any match {
      case v: Type => v
      case v: scala.Seq[_] =>
        org.keymaerax.hippocore.tools.ExprPath(TacticArg.Seq(TacticArg.Int).validateImpl(v).toList)
    }
  }

  case object HippoExpression extends TacticArg {
    override type Type = org.keymaerax.hippocore.proof.HippoExpression
    override def name: String = s"HippoExpression"
    override def validateImpl(any: Any): Type = any match {
      case v: Type => v
      case v => org.keymaerax.hippocore.proof.HippoExpression(TacticArg.Expression.validateImpl(v))
    }
  }

  case object HippoSequent extends TacticArg {
    override type Type = org.keymaerax.hippocore.proof.HippoSequent
    override def name: String = s"HippoSequent"
    override def validateImpl(any: Any): Type = any match {
      case v: Type => v
      case v => org.keymaerax.hippocore.proof.HippoSequent(TacticArg.Sequent.validateImpl(v))
    }
  }

  case object HippoProof extends TacticArg {
    override type Type = org.keymaerax.hippocore.proof.HippoProof
    override def name: String = s"HippoProof"
    override def validateImpl(any: Any): Type = any match {
      case v: Type => v
      case v: ProofInfo => v.proof
    }
  }

  case object ProofInfo extends TacticArg {
    override type Type = org.keymaerax.hippolib.meta.ProofInfo
    override def name: String = s"ProofInfo"
    override def validateImpl(any: Any): Type = any match {
      case v: Type => v
      case v: org.keymaerax.hippocore.proof.HippoProof => org.keymaerax.hippolib.meta.ProofInfo(v)
    }
  }

  case object Name extends TacticArg {
    override type Type = org.keymaerax.hippocore.definitions.Name
    override def name: String = s"Name"
    override def validateImpl(any: Any): Type = any match {
      case v: Type => v
      case v: String => org.keymaerax.hippocore.definitions.Name.parse(v)
    }
  }

  ////////////////////
  // Hippolib types //
  ////////////////////

  case object BelleValue extends TacticArg {
    import org.keymaerax.hippolib.belle.BelleValue as HlBelleValue
    override type Type = HlBelleValue
    override def name: String = s"BelleValue"
    override def validateImpl(any: Any): Type = any match {
      case v: HippoExpression => HlBelleValue.Expression(v.expr, v.defs)
      case v: org.keymaerax.core.Expression => HlBelleValue.Expression(v, Definitions.empty)
      case v: org.keymaerax.core.SubstitutionPair => HlBelleValue.Substitution(v)
      case v: org.keymaerax.infrastruct.PosInExpr => HlBelleValue.PosInExpr(v)
      case v: scala.Int => HlBelleValue.Int(v)
      case v: java.lang.String => HlBelleValue.String(v)
      case v: scala.Option[_] => HlBelleValue.Option(v.map(validateImpl))
      case v: scala.Seq[_] => HlBelleValue.Seq(v.map(validateImpl))
    }
  }
}
