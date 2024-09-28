/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.meta

import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.{HippoException, HippoNonfatalException}

sealed trait TacticArg {
  type Type
  def validate(ctx: HippoContext, any: Any): Type

  @inline
  protected def wrap[T](t: => T): T =
    try t
    catch { case e: HippoException => throw e.annotate(s"while converting to $this") }

  @inline
  protected def fail(value: Any): Nothing = HippoNonfatalException.fail(s"value has type ${value.getClass.getName}")
}

object TacticArg {
  /////////////////
  // Scala types //
  /////////////////

  case object Int extends TacticArg {
    override type Type = scala.Int
    override def validate(ctx: HippoContext, any: Any): Type = wrap {
      any match {
        case v: Type => v
        case v => fail(v)
      }
    }
  }

  case object String extends TacticArg {
    override type Type = java.lang.String
    override def validate(ctx: HippoContext, any: Any): Type = wrap {
      any match {
        case v: Type => v
        case v => fail(v)
      }
    }
  }

  case class Option[A <: TacticArg](inner: A) extends TacticArg {
    override type Type = scala.Option[inner.Type]
    override def validate(ctx: HippoContext, any: Any): Type = wrap {
      any match {
        case v: scala.Option[_] => v.map(inner.validate(ctx, _))
        case v => Some(inner.validate(ctx, v))
      }
    }
  }

  case class Seq[A <: TacticArg](inner: A) extends TacticArg {
    override type Type = scala.Seq[inner.Type]
    override def validate(ctx: HippoContext, any: Any): Type = wrap {
      any match {
        case v: scala.Seq[_] => v.map(inner.validate(ctx, _))
        case v => fail(v)
      }
    }
  }

  case class Tuple2[A1 <: TacticArg, A2 <: TacticArg](inner1: A1, inner2: A2) extends TacticArg {
    override type Type = (inner1.Type, inner2.Type)
    override def validate(ctx: HippoContext, any: Any): Type = wrap {
      any match {
        case (v1, v2) => (inner1.validate(ctx, v1), inner2.validate(ctx, v2))
        case scala.Seq(v1, v2) => (inner1.validate(ctx, v1), inner2.validate(ctx, v2))
        case v => fail(v)
      }
    }
  }

  ////////////////
  // Core types //
  ////////////////

  case object SeqPos extends TacticArg {
    override type Type = org.keymaerax.core.SeqPos
    override def validate(ctx: HippoContext, any: Any): Type = wrap {
      any match {
        case v: Type => v
        case v: Int =>
          HippoNonfatalException.require(v != 0, "SeqPos must not be 0")
          org.keymaerax.core.SeqPos.apply(v)
        case v => fail(v)
      }
    }
  }

  case object AntePos extends TacticArg {
    override type Type = org.keymaerax.core.AntePos
    override def validate(ctx: HippoContext, any: Any): Type = wrap {
      any match {
        case v: Type => v
        case v: Int =>
          // TODO Add AntePos constructor to core?
          HippoNonfatalException.require(v < 0, "AntePos must be smaller than 0")
          org.keymaerax.core.SeqPos.apply(v).asInstanceOf[Type]
        case v => fail(v)
      }
    }
  }

  case object SuccPos extends TacticArg {
    override type Type = org.keymaerax.core.SuccPos
    override def validate(ctx: HippoContext, any: Any): Type = wrap {
      any match {
        case v: Type => v
        case v: Int =>
          // TODO Add SuccPos constructor to core?
          HippoNonfatalException.require(v > 0, "SuccPos must be bigger than 0")
          org.keymaerax.core.SeqPos.apply(v).asInstanceOf[Type]
        case v => fail(v)
      }
    }
  }

  case object Expression extends TacticArg {
    override type Type = org.keymaerax.core.Expression
    override def validate(ctx: HippoContext, any: Any): Type = wrap {
      any match {
        case v: Type => v
        case v => fail(v)
      }
    }
  }

  /////////////////
  // Hippo types //
  /////////////////

  case object ExprPath extends TacticArg {
    override type Type = org.keymaerax.hippolochos.tools.ExprPath
    override def validate(ctx: HippoContext, any: Any): Type = wrap {
      any match {
        case v: Type => v
        case v: scala.Seq[_] =>
          org.keymaerax.hippolochos.tools.ExprPath(TacticArg.Seq(TacticArg.Int).validate(ctx, v).toList)
        case v => fail(v)
      }
    }
  }

  case object HippoProof extends TacticArg {
    override type Type = org.keymaerax.hippolochos.proof.HippoProof
    override def validate(ctx: HippoContext, any: Any): Type = wrap {
      any match {
        case v: Type => v
        case v: ProofInfo => v(ctx)
        case v => fail(v)
      }
    }
  }
}
