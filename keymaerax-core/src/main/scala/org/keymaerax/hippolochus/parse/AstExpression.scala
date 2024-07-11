/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.parse

import org.keymaerax.hippolochus.{HippoBuiltin, HippoExpression, HippoValue}

sealed trait AstExpression
object AstExpression {
  def indentedChildren(exprs: Seq[AstExpression]): String = {
    val result = exprs.map(expr => s"$expr,").mkString("\n").linesIterator.map(line => s"  $line").mkString("\n")
    if (result.isEmpty) result else s"\n$result\n"
  }

  // Primitive
  case class Literal(literal: AstLiteral) extends AstExpression
  case class Theorem(statement: AstExpression, proof: AstExpression.Block) extends AstExpression
  case class On(label: AstLiteral, proof: AstExpression.Block) extends AstExpression
  case class Fork(cases: Seq[AstExpression.On]) extends AstExpression {
    override def toString: String = s"Fork(${indentedChildren(cases)})"
  }
  case class Done() extends AstExpression
  case class Lookup(name: AstIdentifier) extends AstExpression
  case class Assign(name: AstIdentifier, value: AstExpression) extends AstExpression
  case class Parens(exprs: Seq[AstExpression]) extends AstExpression {
    override def toString: String = s"Parens(${indentedChildren(exprs)})"
  }
  case class Block(exprs: Seq[AstExpression]) extends AstExpression {
    override def toString: String = s"Block(${indentedChildren(exprs)})"
  }

  // Suffix
  case class Apply(target: AstExpression, args: IndexedSeq[AstExpression]) extends AstExpression
  case class Access(target: AstExpression, name: AstIdentifier) extends AstExpression

  // Prefix
  case class Not(target: AstExpression) extends AstExpression
  case class Neg(target: AstExpression) extends AstExpression

  // Infix
  case class Mul(left: AstExpression, right: AstExpression) extends AstExpression
  case class Div(left: AstExpression, right: AstExpression) extends AstExpression
  case class Add(left: AstExpression, right: AstExpression) extends AstExpression
  case class Sub(left: AstExpression, right: AstExpression) extends AstExpression
  case class Gt(left: AstExpression, right: AstExpression) extends AstExpression
  case class Gte(left: AstExpression, right: AstExpression) extends AstExpression
  case class Lt(left: AstExpression, right: AstExpression) extends AstExpression
  case class Lte(left: AstExpression, right: AstExpression) extends AstExpression
  case class Eq(left: AstExpression, right: AstExpression) extends AstExpression
  case class Neq(left: AstExpression, right: AstExpression) extends AstExpression
  case class And(left: AstExpression, right: AstExpression) extends AstExpression
  case class Or(left: AstExpression, right: AstExpression) extends AstExpression

  private def convertOnExpr(e: AstExpression.On): HippoExpression.On = HippoExpression
    .On(label = AstLiteral.toHippoValue(e.label), proof = toHippoExpression(e.proof))

  private def convertSeqOfExprs(exprs: Seq[AstExpression]): HippoExpression = {
    val hippoExprs = exprs.map(toHippoExpression)
    hippoExprs.length match {
      case 0 => HippoExpression.Const(HippoValue.Null)
      case 1 => hippoExprs.head
      case _ => hippoExprs.drop(1).foldLeft(hippoExprs.head)(HippoExpression.Sequence)
    }
  }

  private def asBuiltin(builtin: HippoBuiltin, args: IndexedSeq[HippoExpression]): HippoExpression = HippoExpression
    .Apply(HippoExpression.Const(HippoValue.Builtin(builtin)), args)

  def toHippoExpression(expr: AstExpression): HippoExpression = expr match {
    // Primitive
    case e: Literal => HippoExpression.Const(AstLiteral.toHippoValue(e.literal))
    case e: Theorem =>
      HippoExpression.Theorem(statement = toHippoExpression(e.statement), proof = toHippoExpression(e.proof))
    case e: On => convertOnExpr(e)
    case e: Fork => HippoExpression.Fork(cases = e.cases.map(convertOnExpr))
    case _: Done => HippoExpression.Done
    case e: Lookup => HippoExpression.Lookup(name = e.name.name)
    case e: Assign => HippoExpression.Assign(name = e.name.name, value = toHippoExpression(e.value))
    case e: Parens => convertSeqOfExprs(e.exprs)
    case e: Block => HippoExpression.Block(convertSeqOfExprs(e.exprs))
    // Suffix
    case e: Access => HippoExpression.Access(target = toHippoExpression(e.target), name = e.name.name)
    case e: Apply => HippoExpression.Apply(target = toHippoExpression(e.target), args = e.args.map(toHippoExpression))
    // Prefix
    case e: Not => asBuiltin(HippoBuiltin.Not, IndexedSeq(toHippoExpression(e.target)))
    case e: Neg => asBuiltin(HippoBuiltin.Neg, IndexedSeq(toHippoExpression(e.target)))
    // Infix
    case e: Mul => asBuiltin(HippoBuiltin.Mul, IndexedSeq(toHippoExpression(e.left), toHippoExpression(e.right)))
    case e: Div => asBuiltin(HippoBuiltin.Div, IndexedSeq(toHippoExpression(e.left), toHippoExpression(e.right)))
    case e: Add => asBuiltin(HippoBuiltin.Add, IndexedSeq(toHippoExpression(e.left), toHippoExpression(e.right)))
    case e: Sub => asBuiltin(HippoBuiltin.Sub, IndexedSeq(toHippoExpression(e.left), toHippoExpression(e.right)))
    case e: Gt => asBuiltin(HippoBuiltin.Gt, IndexedSeq(toHippoExpression(e.left), toHippoExpression(e.right)))
    case e: Gte => asBuiltin(HippoBuiltin.Gte, IndexedSeq(toHippoExpression(e.left), toHippoExpression(e.right)))
    case e: Lt => asBuiltin(HippoBuiltin.Lt, IndexedSeq(toHippoExpression(e.left), toHippoExpression(e.right)))
    case e: Lte => asBuiltin(HippoBuiltin.Lte, IndexedSeq(toHippoExpression(e.left), toHippoExpression(e.right)))
    case e: Eq => asBuiltin(HippoBuiltin.Eq, IndexedSeq(toHippoExpression(e.left), toHippoExpression(e.right)))
    case e: Neq => asBuiltin(HippoBuiltin.Neq, IndexedSeq(toHippoExpression(e.left), toHippoExpression(e.right)))
    case e: And => asBuiltin(HippoBuiltin.And, IndexedSeq(toHippoExpression(e.left), toHippoExpression(e.right)))
    case e: Or => asBuiltin(HippoBuiltin.Or, IndexedSeq(toHippoExpression(e.left), toHippoExpression(e.right)))
  }
}
