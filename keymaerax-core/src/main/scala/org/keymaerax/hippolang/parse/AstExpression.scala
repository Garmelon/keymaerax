/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.parse

sealed trait AstExpression {
  def slice: SourceFile#Slice
}

object AstExpression {
  ///////////////
  // Primitive //
  ///////////////

  /**
   * {{{
   *   null
   * }}}
   */
  case class Null(slice: SourceFile#Slice) extends AstExpression

  /**
   * {{{
   *   true
   *   false
   * }}}
   */
  case class Bool(slice: SourceFile#Slice, value: scala.Boolean) extends AstExpression

  /**
   * {{{
   *   123
   *   -23_456
   * }}}
   */
  case class Int(slice: SourceFile#Slice, value: scala.Int) extends AstExpression

  /**
   * {{{
   *   "hello world\n"
   * }}}
   */
  case class String(slice: SourceFile#Slice, value: java.lang.String) extends AstExpression

  /**
   * {{{
   *   dLt { 1+1 }
   *   dLt(x, y) { x+y }
   * }}}
   */
  case class DlTerm(slice: SourceFile#Slice, args: Option[Seq[AstIdentifier]], value: org.keymaerax.core.Term)
      extends AstExpression

  /**
   * {{{
   *   dLf { 1+1=2 }
   *   dLf(x, y) { x+y=2 }
   * }}}
   */
  case class DlFormula(slice: SourceFile#Slice, args: Option[Seq[AstIdentifier]], value: org.keymaerax.core.Formula)
      extends AstExpression

  /**
   * {{{
   *   dLfp { [a;]x }
   *   dLfp(x) { [a;]x }
   * }}}
   */
  case class DlFormulaPredicational(
      slice: SourceFile#Slice,
      arg: Option[AstIdentifier],
      value: org.keymaerax.core.Formula,
  ) extends AstExpression

  /**
   * {{{
   *   dLp { x:=2; }
   * }}}
   */
  case class DlProgram(slice: SourceFile#Slice, value: org.keymaerax.core.Program) extends AstExpression

  /**
   * {{{
   *   dLs { ==> 1+1=2 }
   * }}}
   */
  case class DlSequent(slice: SourceFile#Slice, value: org.keymaerax.core.Sequent) extends AstExpression

  /**
   * {{{
   *   #not
   * }}}
   */
  case class BuiltinFunction(slice: SourceFile#Slice, value: org.keymaerax.hippolang.BuiltinFunction)
      extends AstExpression

  /**
   * {{{
   *   import <path>
   * }}}
   */
  case class Import(slice: SourceFile#Slice, path: AstExpression) extends AstExpression

  /**
   * {{{
   * val <name> = <value>
   * var <name> = <value>
   * export val <name> = <value>
   * export var <name> = <value>
   * }}}
   */
  case class Declare(
      slice: SourceFile#Slice,
      exportSlice: Option[SourceFile#Slice],
      mutable: Boolean,
      name: AstIdentifier,
      value: AstExpression,
  ) extends AstExpression

  /**
   * {{{
   * <name> = <value>
   * }}}
   */
  case class Assign(slice: SourceFile#Slice, name: AstIdentifier, value: AstExpression) extends AstExpression

  /**
   * {{{
   * <name>
   * }}}
   */
  case class Lookup(slice: SourceFile#Slice, name: AstIdentifier) extends AstExpression

  /**
   * {{{
   *   if (<condition>) <ifTrue>
   *   if (<condition>) <ifTrue> else <ifFalse>
   * }}}
   */
  case class If(
      slice: SourceFile#Slice,
      condition: AstExpression.Parens,
      ifTrue: AstExpression,
      ifFalse: Option[AstExpression],
  ) extends AstExpression

  /**
   * {{{
   *   while (<condition>) <body>
   * }}}
   */
  case class While(slice: SourceFile#Slice, condition: AstExpression.Parens, body: AstExpression) extends AstExpression

  /**
   * {{{
   *   function(a, b) a + b
   * }}}
   */
  case class Function(slice: SourceFile#Slice, args: Seq[AstIdentifier], body: AstExpression) extends AstExpression

  /**
   * {{{
   *   theorem <conclusion>
   *   premise <premise> ...
   *   by <proof>
   * }}}
   */
  case class Theorem(
      slice: SourceFile#Slice,
      verifySlice: Option[SourceFile#Slice],
      conclusion: AstExpression,
      premises: Seq[AstExpression],
      proof: AstExpression,
      proofSlice: SourceFile#Slice,
  ) extends AstExpression

  /**
   * {{{
   *   (<expr>; <expr>;)
   *   (<expr>; <expr>; <returnExpr>)
   * }}}
   */
  case class Parens(slice: SourceFile#Slice, exprs: Seq[AstExpression], returnExpr: Option[AstExpression])
      extends AstExpression

  /**
   * {{{
   *   {<expr>; <expr>;}
   *   {<expr>; <expr>; <returnExpr>}
   * }}}
   */
  case class Block(slice: SourceFile#Slice, exprs: Seq[AstExpression], returnExpr: Option[AstExpression])
      extends AstExpression

  /**
   * {{{
   *   backward { ... }
   * }}}
   */
  case class BackwardBlock(slice: SourceFile#Slice, inner: Block) extends AstExpression

  /**
   * {{{
   *   graph { ... }
   * }}}
   */
  case class GraphBlock(slice: SourceFile#Slice, inner: Block) extends AstExpression

  ////////////
  // Suffix //
  ////////////

  /**
   * {{{
   *   <target>.#<member>
   * }}}
   */
  case class BuiltinAccess(
      slice: SourceFile#Slice,
      target: AstExpression,
      member: org.keymaerax.hippolang.BuiltinMemberFunction,
  ) extends AstExpression

  /**
   * {{{
   *   <target>.<name>
   * }}}
   */
  case class Access(slice: SourceFile#Slice, target: AstExpression, name: AstIdentifier) extends AstExpression

  /**
   * {{{
   *   <target>(<args>)
   * }}}
   */
  case class Apply(
      slice: SourceFile#Slice,
      target: AstExpression,
      args: IndexedSeq[AstExpression],
      argsSlice: SourceFile#Slice,
  ) extends AstExpression

  /**
   * {{{
   *   <target>[<args>]
   * }}}
   */
  case class ApplyTactic(
      slice: SourceFile#Slice,
      target: AstExpression,
      args: IndexedSeq[AstExpression],
      argsSlice: SourceFile#Slice,
  ) extends AstExpression

  ////////////
  // Prefix //
  ////////////

  /**
   * {{{
   *   !<target>
   * }}}
   */
  case class Not(slice: SourceFile#Slice, opSlice: SourceFile#Slice, target: AstExpression) extends AstExpression

  /**
   * {{{
   *   -<target>
   * }}}
   */
  case class Neg(slice: SourceFile#Slice, opSlice: SourceFile#Slice, target: AstExpression) extends AstExpression

  ///////////
  // Infix //
  ///////////

  /**
   * {{{
   *   <left> * <right>
   * }}}
   */
  case class Mul(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   <left> / <right>
   * }}}
   */
  case class Div(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   <left> + <right>
   * }}}
   */
  case class Add(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   <left> - <right>
   * }}}
   */
  case class Sub(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   <left> > <right>
   * }}}
   */
  case class Gt(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   <left> >= <right>
   * }}}
   */
  case class Gte(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   <left> < <right>
   * }}}
   */
  case class Lt(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   <left> <= <right>
   * }}}
   */
  case class Lte(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   <left> == <right>
   * }}}
   */
  case class Eq(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   <left> != <right>
   * }}}
   */
  case class Neq(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   <left> && <right>
   * }}}
   */
  case class And(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   <left> || <right>
   * }}}
   */
  case class Or(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   <left> -> <right>
   * }}}
   */
  case class MapsTo(slice: SourceFile#Slice, opSlice: SourceFile#Slice, left: AstExpression, right: AstExpression)
      extends AstExpression
}
