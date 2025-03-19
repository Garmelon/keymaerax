/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.core.{Expression, Sequent}
import org.keymaerax.hippolang.namespace.ImmutableNamespace
import org.keymaerax.hippolang.parse.{AstExpression, AstIdentifier, SourceFile}
import org.keymaerax.hippolib.meta.{ProofInfo, TacticInfo}
import org.keymaerax.hippolochos
import org.keymaerax.hippolochos.proof.HippoProof

object HippoConversions {
  //////////////////////////////
  // Converting to HippoValue //
  //////////////////////////////

  implicit class OptionConversion(val it: Option[HippoValue]) {
    def toHValue: HippoValue = it.getOrElse(HippoValue.Null)
  }

  implicit class BooleanConversion(val it: Boolean) {
    def toHValue: HippoValue.Bool = HippoValue.Bool(it)
  }

  implicit class IntConversion(val it: Int) {
    def toHValue: HippoValue.Int = HippoValue.Int(it)
  }

  implicit class StringConversion(val it: String) {
    def toHValue: HippoValue.String = HippoValue.String(it)
  }

  implicit class ListConversion(val it: IndexedSeq[HippoValue]) {
    def toHValue: HippoValue.List = HippoValue.List(it)
  }

  implicit class ExpressionConversion(val it: Expression) {
    def toHValue: HippoValue.DlExpression = HippoValue.DlExpression(it)
  }

  implicit class SequentConversion(val it: Sequent) {
    def toHValue: HippoValue.DlSequent = HippoValue.DlSequent(it)
  }

  implicit class HippoBuiltinFunctionConversion(val it: BuiltinFunction) {
    def toHValue: HippoValue.BuiltinFunction = HippoValue.BuiltinFunction(it)
  }

  implicit class ImmutableNamespaceConversion(val it: ImmutableNamespace) {
    def toHValue: HippoValue.Namespace = HippoValue.Namespace(it)
  }

  implicit class HippoProofConversion(val it: HippoProof) {
    def toHValue: HippoValue.Proof = HippoValue.Proof(it)
  }

  implicit class TacticConversion(val it: hippolochos.Tactic) {
    def toHValue: HippoValue.Tactic = HippoValue.Tactic(it)
  }

  implicit class HippoIdentifierConversion(val it: HippoIdentifier) {
    def toHValue: HippoValue.String = it.value.toHValue
  }

  implicit class ProofInfoConversion(val it: ProofInfo) {
    def toHValue: HippoValue.ProofInfo = HippoValue.ProofInfo(it)
  }

  implicit class TacticInfoConversion(val it: TacticInfo) {
    def toHValue: HippoValue.TacticInfo = HippoValue.TacticInfo(it)
  }

  implicit class AstIdentifierConversion(val it: AstIdentifier) {
    def toHValue: HippoValue.String = it.name.toHValue
  }

  ///////////////////////////////////
  // Converting to HippoExpression //
  ///////////////////////////////////

  implicit class HippoValueConversion(val it: HippoValue) {
    def toHExpr(slice: SourceFile#Slice): HippoExpression.Const = HippoExpression.Const(slice, it)
  }

  implicit class AstExpressionConversion(val it: AstExpression) {
    import org.keymaerax.hippolang.HippoExpression.*

    def toHExpr: HippoExpression = it match {
      case e: AstExpression.Null => HippoValue.Null.toHExpr(e.slice)
      case e: AstExpression.Bool => HippoValue.Bool(e.value).toHExpr(e.slice)
      case e: AstExpression.Int => HippoValue.Int(e.value).toHExpr(e.slice)
      case e: AstExpression.String => HippoValue.String(e.value).toHExpr(e.slice)
      case e: AstExpression.DlTerm => HippoValue.DlExpression(e.value).toHExpr(e.slice)
      case e: AstExpression.DlFormula => HippoValue.DlExpression(e.value).toHExpr(e.slice)
      case e: AstExpression.DlProgram => HippoValue.DlExpression(e.value).toHExpr(e.slice)
      case e: AstExpression.DlExpression => HippoValue.DlExpression(e.value).toHExpr(e.slice)
      case e: AstExpression.DlSequent => HippoValue.DlSequent(e.value).toHExpr(e.slice)
      case e: AstExpression.BuiltinFunction => HippoValue.BuiltinFunction(e.value).toHExpr(e.slice)
      case e: AstExpression.Import => Import(slice = e.slice, path = e.path.toHExpr)
      case e: AstExpression.Declare => Declare(
          slice = e.slice,
          exportSlice = e.exportSlice,
          mutable = e.mutable,
          name = e.name.name,
          value = e.value.toHExpr,
        )
      case e: AstExpression.Assign => Assign(slice = e.slice, name = e.name.name, value = e.value.toHExpr)
      case e: AstExpression.Lookup => Lookup(slice = e.slice, name = e.name.name)
      case e: AstExpression.If => If(
          slice = e.slice,
          condition = e.condition.toHExpr,
          ifTrue = e.ifTrue.toHExpr,
          ifFalse = e.ifFalse.map(_.toHExpr),
        )
      case e: AstExpression.While => While(slice = e.slice, condition = e.condition.toHExpr, body = e.body.toHExpr)
      case e: AstExpression.Function => Function(slice = e.slice, args = e.args.map(_.name), body = e.body.toHExpr)
      case e: AstExpression.Theorem => Theorem(
          slice = e.slice,
          verifySlice = e.verifySlice,
          conclusion = e.conclusion.toHExpr,
          premises = e.premises.map(_.toHExpr),
          proof = e.proof.toHExpr,
          proofSlice = e.proofSlice,
        )
      case e: AstExpression.Parens =>
        Sequence(slice = e.slice, exprs = e.exprs.map(_.toHExpr), returnExpr = e.returnExpr.map(_.toHExpr))
      case e: AstExpression.Block => Block(
          slice = e.slice,
          Sequence(slice = e.slice, exprs = e.exprs.map(_.toHExpr), returnExpr = e.returnExpr.map(_.toHExpr)),
        )
      case e: AstExpression.BackwardBlock => BackwardBlock(slice = e.slice, inner = e.inner.toHExpr)
      case e: AstExpression.GraphBlock => GraphBlock(slice = e.slice, inner = e.inner.toHExpr)
      case e: AstExpression.BuiltinAccess =>
        BuiltinAccess(slice = e.slice, target = e.target.toHExpr, member = e.member)
      case e: AstExpression.Access => Access(slice = e.slice, target = e.target.toHExpr, name = e.name.name)
      case e: AstExpression.Apply =>
        Apply(slice = e.slice, target = e.target.toHExpr, args = e.args.map(_.toHExpr), argsSlice = e.argsSlice)
      case e: AstExpression.ApplyTactic =>
        ApplyTactic(slice = e.slice, target = e.target.toHExpr, args = e.args.map(_.toHExpr), argsSlice = e.argsSlice)
      case e: AstExpression.Not => Apply(
          slice = e.slice,
          target = BuiltinFunction.Not.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.target.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.Neg => Apply(
          slice = e.slice,
          target = BuiltinFunction.Neg.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.target.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.Mul => Apply(
          slice = e.slice,
          target = BuiltinFunction.Mul.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.Div => Apply(
          slice = e.slice,
          target = BuiltinFunction.Div.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.Add => Apply(
          slice = e.slice,
          target = BuiltinFunction.Add.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.Sub => Apply(
          slice = e.slice,
          target = BuiltinFunction.Sub.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.Gt => Apply(
          slice = e.slice,
          target = BuiltinFunction.Gt.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.Gte => Apply(
          slice = e.slice,
          target = BuiltinFunction.Gte.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.Lt => Apply(
          slice = e.slice,
          target = BuiltinFunction.Lt.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.Lte => Apply(
          slice = e.slice,
          target = BuiltinFunction.Lte.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.Eq => Apply(
          slice = e.slice,
          target = BuiltinFunction.Eq.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.Neq => Apply(
          slice = e.slice,
          target = BuiltinFunction.Neq.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.And => Apply(
          slice = e.slice,
          target = BuiltinFunction.And.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.Or => Apply(
          slice = e.slice,
          target = BuiltinFunction.Or.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
      case e: AstExpression.MapsTo => Apply(
          slice = e.slice,
          target = BuiltinFunction.List.toHValue.toHExpr(e.opSlice),
          args = IndexedSeq(e.left.toHExpr, e.right.toHExpr),
          argsSlice = e.slice,
        )
    }
  }
}
