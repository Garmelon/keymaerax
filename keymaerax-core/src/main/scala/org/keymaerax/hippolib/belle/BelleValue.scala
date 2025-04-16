/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.belle

import org.keymaerax.hippocore.tools.{Hashable, Hasher}

/**
 * Types that we might want to feed to a Bellerophon tactic, roughly modeled after
 * [[org.keymaerax.btactics.macros.ArgInfo]].
 *
 * This type exists (instead of using a plain [[Any]] for tactic arguments directly) because we can't hash [[Any]] in
 * general. Such a hash algorithm would have to throw errors when it encounters types it doesn't know how to hash.
 * Instead, we want to error when converting from [[Any]] during tactic argument parsing in the
 * [[org.keymaerax.hippolib.meta.TacticConstructor]], where errors are already expected.
 */
sealed trait BelleValue extends Hashable
object BelleValue {
  ///////////////////////
  // KeYmaera X values //
  ///////////////////////

  case class Expression(value: org.keymaerax.core.Expression) extends BelleValue {
    override def digestInto(hasher: Hasher): Unit = hasher.digest("Expression").digest(value)
  }

  case class Substitution(value: org.keymaerax.core.SubstitutionPair) extends BelleValue {
    override def digestInto(hasher: Hasher): Unit = hasher.digest("Substitution").digest(value)
  }

  case class PosInExpr(value: org.keymaerax.infrastruct.PosInExpr) extends BelleValue {
    override def digestInto(hasher: Hasher): Unit = hasher.digest("PosInExpr").digestSeqWith(value.pos)(_.digest(_))
  }

  //////////////////
  // Scala values //
  //////////////////

  case class Int(value: scala.Int) extends BelleValue {
    override def digestInto(hasher: Hasher): Unit = hasher.digest("Int").digest(value)
  }

  case class String(value: java.lang.String) extends BelleValue {
    override def digestInto(hasher: Hasher): Unit = hasher.digest("String").digest(value)
  }

  case class Option(value: scala.Option[BelleValue]) extends BelleValue {
    override def digestInto(hasher: Hasher): Unit = hasher.digest("Option").digestOpt(value)
  }

  case class Seq(value: scala.Seq[BelleValue]) extends BelleValue {
    override def digestInto(hasher: Hasher): Unit = hasher.digest("Seq").digestSeq(value)
  }
}
