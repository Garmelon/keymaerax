/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.core.{Formula, Sequent}
import org.keymaerax.hippolang.namespace.ImmutableNamespace
import org.keymaerax.hippolang.parse.{AstExpression, AstIdentifier}
import org.keymaerax.hippolib.meta.{ProofInfo, TacticInfo}
import org.keymaerax.hippolochos
import org.keymaerax.hippolochos.proof.HippoProof

object HippoConversions {
  //////////////////////////////
  // Converting to HippoValue //
  //////////////////////////////

  implicit class OptionConversion(val it: Option[HippoValue]) {
    def toHValue: HippoValue = it.getOrElse(HippoValue.Null)
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class BooleanConversion(val it: Boolean) {
    def toHValue: HippoValue.Bool = HippoValue.Bool(it)
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class IntConversion(val it: Int) {
    def toHValue: HippoValue.Int = HippoValue.Int(it)
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class StringConversion(val it: String) {
    def toHValue: HippoValue.String = HippoValue.String(it)
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class FormulaConversion(val it: Formula) {
    def toHValue: HippoValue.DlExpression = HippoValue.DlExpression(it)
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class SequentConversion(val it: Sequent) {
    def toHValue: HippoValue.DlSequent = HippoValue.DlSequent(it)
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class HippoBuiltinFunctionConversion(val it: BuiltinFunction) {
    def toHValue: HippoValue.BuiltinFunction = HippoValue.BuiltinFunction(it)
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class ImmutableNamespaceConversion(val it: ImmutableNamespace) {
    def toHValue: HippoValue.Namespace = HippoValue.Namespace(it)
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class HippoProofConversion(val it: HippoProof) {
    def toHValue: HippoValue.Proof = HippoValue.Proof(it)
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class TacticConversion(val it: hippolochos.Tactic) {
    def toHValue: HippoValue.Tactic = HippoValue.Tactic(it)
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class HippoIdentifierConversion(val it: HippoIdentifier) {
    def toHValue: HippoValue.String = it.value.toHValue
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class ProofInfoConversion(val it: ProofInfo) {
    def toHValue: HippoValue.ProofInfo = HippoValue.ProofInfo(it)
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class TacticInfoConversion(val it: TacticInfo) {
    def toHValue: HippoValue.TacticInfo = HippoValue.TacticInfo(it)
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  implicit class AstIdentifierConversion(val it: AstIdentifier) {
    def toHValue: HippoValue.String = it.name.toHValue
    def toHExpr: HippoExpression.Const = toHValue.toHExpr
  }

  ///////////////////////////////////
  // Converting to HippoExpression //
  ///////////////////////////////////

  implicit class HippoValueConversion(val it: HippoValue) {
    def toHExpr: HippoExpression.Const = HippoExpression.Const(it)
  }

  implicit class AstExpressionConversion(val it: AstExpression) {
    import org.keymaerax.hippolang.HippoExpression._

    def toHExpr: HippoExpression = it match {
      case AstExpression.Null() => HippoValue.Null.toHExpr
      case AstExpression.Bool(value) => HippoValue.Bool(value).toHExpr
      case AstExpression.Int(value) => HippoValue.Int(value).toHExpr
      case AstExpression.String(value) => HippoValue.String(value).toHExpr
      case AstExpression.DlExpression(value) => HippoValue.DlExpression(value).toHExpr
      case AstExpression.DlSequent(value) => HippoValue.DlSequent(value).toHExpr
      case AstExpression.BuiltinFunction(value) => HippoValue.BuiltinFunction(value).toHExpr
      case e: AstExpression.Import => Import(path = e.path.toHExpr, slice = e.slice)
      case e: AstExpression.Declare =>
        Declare(exportSlice = e.exportSlice, mutable = e.mutable, name = e.name.name, value = e.value.toHExpr)
      case AstExpression.Assign(name, value) => Assign(name.name, value.toHExpr)
      case AstExpression.Lookup(name) => Lookup(name.name)
      case AstExpression.If(condition, ifTrue, ifFalse) => If(condition.toHExpr, ifTrue.toHExpr, ifFalse.map(_.toHExpr))
      case AstExpression.While(condition, body) => While(condition.toHExpr, body.toHExpr)
      case AstExpression.Function(args, body) => Function(args.map(_.name), body.toHExpr)
      case e: AstExpression.Theorem => Theorem(
          verifySlice = e.verifySlice,
          conclusion = e.conclusion.toHExpr,
          premises = e.premises.map(_.toHExpr),
          proof = e.proof.toHExpr,
          proofSlice = e.proofSlice,
        )
      case AstExpression.Parens(exprs, returnExpr) => Sequence(exprs.map(_.toHExpr), returnExpr.map(_.toHExpr))
      case AstExpression.Block(exprs, returnExpr) => Block(Sequence(exprs.map(_.toHExpr), returnExpr.map(_.toHExpr)))
      case AstExpression.BackwardBlock(inner) => BackwardBlock(inner.toHExpr)
      case AstExpression.GraphBlock(inner) => GraphBlock(inner.toHExpr)
      case AstExpression.BuiltinAccess(target, member) => BuiltinAccess(target.toHExpr, member)
      case AstExpression.Access(target, name) => Access(target.toHExpr, name.name)
      case AstExpression.Apply(target, args) => Apply(target.toHExpr, args.map(_.toHExpr))
      case AstExpression.ApplyTactic(target, args) => ApplyTactic(target.toHExpr, args.map(_.toHExpr))
      case AstExpression.Not(target) => Apply(BuiltinFunction.Not.toHExpr, target.toHExpr)
      case AstExpression.Neg(target) => Apply(BuiltinFunction.Neg.toHExpr, target.toHExpr)
      case AstExpression.Mul(left, right) => Apply(BuiltinFunction.Mul.toHExpr, left.toHExpr, right.toHExpr)
      case AstExpression.Div(left, right) => Apply(BuiltinFunction.Div.toHExpr, left.toHExpr, right.toHExpr)
      case AstExpression.Add(left, right) => Apply(BuiltinFunction.Add.toHExpr, left.toHExpr, right.toHExpr)
      case AstExpression.Sub(left, right) => Apply(BuiltinFunction.Sub.toHExpr, left.toHExpr, right.toHExpr)
      case AstExpression.Gt(left, right) => Apply(BuiltinFunction.Gt.toHExpr, left.toHExpr, right.toHExpr)
      case AstExpression.Gte(left, right) => Apply(BuiltinFunction.Gte.toHExpr, left.toHExpr, right.toHExpr)
      case AstExpression.Lt(left, right) => Apply(BuiltinFunction.Lt.toHExpr, left.toHExpr, right.toHExpr)
      case AstExpression.Lte(left, right) => Apply(BuiltinFunction.Lte.toHExpr, left.toHExpr, right.toHExpr)
      case AstExpression.Eq(left, right) => Apply(BuiltinFunction.Eq.toHExpr, left.toHExpr, right.toHExpr)
      case AstExpression.Neq(left, right) => Apply(BuiltinFunction.Neq.toHExpr, left.toHExpr, right.toHExpr)
      case AstExpression.And(left, right) => Apply(BuiltinFunction.And.toHExpr, left.toHExpr, right.toHExpr)
      case AstExpression.Or(left, right) => Apply(BuiltinFunction.Or.toHExpr, left.toHExpr, right.toHExpr)
      case AstExpression.MapsTo(left, right) => Apply(BuiltinFunction.List.toHExpr, left.toHExpr, right.toHExpr)
    }
  }
}
