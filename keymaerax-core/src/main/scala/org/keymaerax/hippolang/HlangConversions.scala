/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.hippocore
import org.keymaerax.hippocore.proof.{HippoExpression, HippoProof, HippoSequent}
import org.keymaerax.hippolang.interpret.Interpolator
import org.keymaerax.hippolang.namespace.ImmutableNamespace
import org.keymaerax.hippolang.parse.{AstExpression, AstIdentifier, SourceFile}
import org.keymaerax.hippolib.meta.{ProofInfo, TacticInfo}

object HlangConversions {
  //////////////////////////////
  // Converting to HippoValue //
  //////////////////////////////

  implicit class OptionConversion(val it: Option[HlangValue]) {
    def toHValue: HlangValue = it.getOrElse(HlangValue.Null)
  }

  implicit class BooleanConversion(val it: Boolean) {
    def toHValue: HlangValue.Bool = HlangValue.Bool(it)
  }

  implicit class IntConversion(val it: Int) {
    def toHValue: HlangValue.Int = HlangValue.Int(it)
  }

  implicit class StringConversion(val it: String) {
    def toHValue: HlangValue.String = HlangValue.String(it)
  }

  implicit class ListConversion(val it: IndexedSeq[HlangValue]) {
    def toHValue: HlangValue.List = HlangValue.List(it)
  }

  implicit class HippoExpressionConversion(val it: HippoExpression) {
    def toHValue: HlangValue.DlExpression = HlangValue.DlExpression(it)
  }

  implicit class HippoSequentConversion(val it: HippoSequent) {
    def toHValue: HlangValue.DlSequent = HlangValue.DlSequent(it)
  }

  implicit class HippoBuiltinFunctionConversion(val it: BuiltinFunction) {
    def toHValue: HlangValue.BuiltinFunction = HlangValue.BuiltinFunction(it)
  }

  implicit class ImmutableNamespaceConversion(val it: ImmutableNamespace) {
    def toHValue: HlangValue.Namespace = HlangValue.Namespace(it)
  }

  implicit class HippoProofConversion(val it: HippoProof) {
    def toHValue: HlangValue.Proof = HlangValue.Proof(it)
  }

  implicit class TacticConversion(val it: hippocore.Tactic) {
    def toHValue: HlangValue.Tactic = HlangValue.Tactic(it)
  }

  implicit class HippoIdentifierConversion(val it: HlangIdentifier) {
    def toHValue: HlangValue.String = it.value.toHValue
  }

  implicit class ProofInfoConversion(val it: ProofInfo) {
    def toHValue: HlangValue.ProofInfo = HlangValue.ProofInfo(it)
  }

  implicit class TacticInfoConversion(val it: TacticInfo) {
    def toHValue: HlangValue.TacticInfo = HlangValue.TacticInfo(it)
  }

  implicit class AstIdentifierConversion(val it: AstIdentifier) {
    def toHValue: HlangValue.String = it.name.toHValue
  }

  ///////////////////////////////////
  // Converting to HippoExpression //
  ///////////////////////////////////

  implicit class HippoValueConversion(val it: HlangValue) {
    def toHExpr(slice: SourceFile#Slice): HlangExpression.Const = HlangExpression.Const(slice, it)
  }

  implicit class AstExpressionConversion(val it: AstExpression) {
    import org.keymaerax.hippolang.HlangExpression.*

    def toHExpr: HlangExpression = it match {
      case e: AstExpression.Null => HlangValue.Null.toHExpr(e.slice)
      case e: AstExpression.Bool => HlangValue.Bool(e.value).toHExpr(e.slice)
      case e: AstExpression.Int => HlangValue.Int(e.value).toHExpr(e.slice)
      case e: AstExpression.String => HlangValue.String(e.value).toHExpr(e.slice)
      case e: AstExpression.DlTerm =>
        val value = e.args match {
          case None => e.value
          case Some(args) => Interpolator.replaceFuncArgs(args.map(_.name.value), e.value)
        }
        HlangExpression.DlExpression(e.slice, value)
      case e: AstExpression.DlFormula =>
        val value = e.args match {
          case None => e.value
          case Some(args) => Interpolator.replacePredArgs(args.map(_.name.value), e.value)
        }
        HlangExpression.DlExpression(e.slice, value)
      case e: AstExpression.DlFormulaPredicational =>
        val value = e.arg match {
          case None => e.value
          case Some(arg) => Interpolator.replacePredicationalArg(arg.name.value, e.value)
        }
        HlangExpression.DlExpression(e.slice, value)
      case e: AstExpression.DlProgram => HlangExpression.DlExpression(e.slice, e.value)
      case e: AstExpression.DlSequent => HlangExpression.DlSequent(e.slice, e.value)
      case e: AstExpression.BuiltinFunction => HlangValue.BuiltinFunction(e.value).toHExpr(e.slice)
      case e: AstExpression.Import => Import(slice = e.slice, path = e.path.toHExpr)
      case e: AstExpression.Declare => Declare(
          slice = e.slice,
          exportSlice = e.exportSlice,
          mutable = e.mutable,
          name = e.name.name,
          value = e.value.toHExpr,
        )
      case e: AstExpression.Assign => Assign(slice = e.slice, name = e.name.name, value = e.value.toHExpr)
      case e: AstExpression.AssignGoal =>
        AssignGoal(slice = e.slice, nameSlice = e.nameSlice, name = e.name.name, value = e.value.toHExpr)
      case e: AstExpression.Lookup => Lookup(slice = e.slice, name = e.name.name)
      case e: AstExpression.LookupGoal => LookupGoal(slice = e.slice, name = e.name.name)
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
      case e: AstExpression.BackwardBlock => BackwardBlock(
          slice = e.slice,
          premises = e.premises.map(_.name),
          conclusion = e.conclusion.name,
          inner = e.inner.toHExpr,
        )
      case e: AstExpression.BackwardAssign =>
        val goal = HlangIdentifier("conclusion")
        val assignValueSlice = e.assignSlice + e.value.slice
        BackwardBlock(
          slice = e.slice,
          premises = e.premises.map(_.name),
          conclusion = goal,
          inner = Block(
            slice = assignValueSlice,
            inner = Sequence(
              slice = assignValueSlice,
              exprs = Seq(
                AssignGoal(slice = assignValueSlice, nameSlice = e.assignSlice, name = goal, value = e.value.toHExpr)
              ),
              returnExpr = None,
            ),
          ),
        )
      case e: AstExpression.GraphBlock => GraphBlock(slice = e.slice, inner = e.inner.toHExpr)
      case e: AstExpression.BuiltinAccess =>
        BuiltinAccess(slice = e.slice, target = e.target.toHExpr, member = e.member)
      case e: AstExpression.Access =>
        Access(slice = e.slice, nameSlice = e.nameSlice, target = e.target.toHExpr, name = e.name.name)
      case e: AstExpression.Apply =>
        Apply(slice = e.slice, target = e.target.toHExpr, args = e.args.map(_.toHExpr), argsSlice = e.argsSlice)
      case e: AstExpression.ApplyTactic =>
        ApplyTactic(slice = e.slice, target = e.target.toHExpr, args = e.args.map(_.toHExpr), argsSlice = e.argsSlice)
      case e: AstExpression.PipeLeftTactic => convertPipeLeftTactic(e)
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

    private def convertPipeLeftTactic(e: AstExpression.PipeLeftTactic): HlangExpression = e.target match {
      case t: AstExpression.ApplyTactic =>
        def isPlaceholder(e: AstExpression): Boolean = e match {
          case arg: AstExpression.Lookup if arg.name == AstIdentifier("_") => true
          case _ => false
        }

        val placeholderI = t.args.indexWhere(isPlaceholder)
        val lastPlaceholderI = t.args.lastIndexWhere(isPlaceholder)
        if (placeholderI < 0 || placeholderI != lastPlaceholderI)
          throw HlangException("Parsing failed", t.argsSlice, "expected exactly one placeholder _")

        val beforePlaceholder = t.args.take(placeholderI).map(_.toHExpr)
        val afterPlaceholder = t.args.drop(placeholderI + 1).map(_.toHExpr)
        val placeholder = e.arg.toHExpr

        ApplyTactic(
          slice = e.slice,
          argsSlice = t.argsSlice,
          target = t.target.toHExpr,
          args = (beforePlaceholder :+ placeholder) ++ afterPlaceholder,
        )

      case _ => ApplyTactic(
          slice = e.slice,
          argsSlice = e.opSlice + e.arg.slice,
          target = e.target.toHExpr,
          args = IndexedSeq(e.arg.toHExpr),
        )
    }
  }
}
