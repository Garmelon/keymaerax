/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

sealed trait TacticArg {
  type Type
  def validate(any: Any): Type
}

object TacticArg {
  /////////////////
  // Scala types //
  /////////////////

  case object Int extends TacticArg {
    override type Type = scala.Int
    override def validate(any: Any): Type = any.asInstanceOf[Type]
  }

  case object String extends TacticArg {
    override type Type = java.lang.String
    override def validate(any: Any): Type = any.asInstanceOf[Type]
  }

  case class Option[A <: TacticArg](inner: A) extends TacticArg {
    override type Type = scala.Option[inner.Type]
    override def validate(any: Any): Type = any match {
      case v: scala.Option[_] => v.map(inner.validate)
      case v => Some(inner.validate(v))
    }
  }

  case class Seq[A <: TacticArg](inner: A) extends TacticArg {
    override type Type = scala.Seq[inner.Type]
    override def validate(any: Any): Type = any.asInstanceOf[scala.Seq[_]].map(inner.validate)
  }

  case class Tuple2[A1 <: TacticArg, A2 <: TacticArg](inner1: A1, inner2: A2) extends TacticArg {
    override type Type = (inner1.Type, inner2.Type)
    override def validate(any: Any): Type = any match {
      case (v1, v2) => (inner1.validate(v1), inner2.validate(v2))
      case scala.Seq(v1, v2) => (inner1.validate(v1), inner2.validate(v2))
    }
  }

  ////////////////
  // Core types //
  ////////////////

  case object SeqPos extends TacticArg {
    override type Type = org.keymaerax.core.SeqPos
    override def validate(any: Any): Type = any match {
      case v: Type => v
      case v: Int =>
        require(v != 0, "SeqPos must not be 0")
        org.keymaerax.core.SeqPos.apply(v)
    }
  }

  case object AntePos extends TacticArg {
    override type Type = org.keymaerax.core.AntePos
    override def validate(any: Any): Type = any match {
      case v: Type => v
      case v: Int =>
        // TODO Add AntePos constructor to core?
        require(v < 0, "AntePos must be smaller than 0")
        org.keymaerax.core.SeqPos.apply(v).asInstanceOf[Type]
    }
  }

  case object SuccPos extends TacticArg {
    override type Type = org.keymaerax.core.SuccPos
    override def validate(any: Any): Type = any match {
      case v: Type => v
      case v: Int =>
        // TODO Add SuccPos constructor to core?
        require(v > 0, "SuccPos must be bigger than 0")
        org.keymaerax.core.SeqPos.apply(v).asInstanceOf[Type]
    }
  }

  case object Expression extends TacticArg {
    override type Type = org.keymaerax.core.Expression
    override def validate(any: Any): Type = any.asInstanceOf[Type]
  }

  case object Formula extends TacticArg {
    override type Type = org.keymaerax.core.Formula
    override def validate(any: Any): Type = any.asInstanceOf[Type]
  }

  case object Sequent extends TacticArg {
    override type Type = org.keymaerax.core.Sequent
    override def validate(any: Any): Type = any.asInstanceOf[Type]
  }

  case object Variable extends TacticArg {
    override type Type = org.keymaerax.core.Variable
    override def validate(any: Any): Type = any.asInstanceOf[Type]
  }

  /////////////////
  // Hippo types //
  /////////////////

  case object ExprPath extends TacticArg {
    override type Type = org.keymaerax.hippocore.tools.ExprPath
    override def validate(any: Any): Type = any match {
      case v: Type => v
      case v: scala.Seq[_] => org.keymaerax.hippocore.tools.ExprPath(TacticArg.Seq(TacticArg.Int).validate(v).toList)
    }
  }

  case object HippoProof extends TacticArg {
    override type Type = org.keymaerax.hippocore.proof.HippoProof
    override def validate(any: Any): Type = any match {
      case v: Type => v
      case v: ProofInfo => v.proof
    }
  }

  case object ProofInfo extends TacticArg {
    override type Type = org.keymaerax.hippolib.meta.ProofInfo
    override def validate(any: Any): Type = any match {
      case v: Type => v
      case v: org.keymaerax.hippocore.proof.HippoProof => org.keymaerax.hippolib.meta.ProofInfo(v)
    }
  }
}
