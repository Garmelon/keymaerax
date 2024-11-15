/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.parse

sealed trait AstExpression
object AstExpression {
  ///////////////
  // Primitive //
  ///////////////

  /**
   * {{{
   *   null
   * }}}
   */
  case class Null() extends AstExpression

  /**
   * {{{
   *   true
   *   false
   * }}}
   */
  case class Bool(value: scala.Boolean) extends AstExpression

  /**
   * {{{
   *   123
   *   -23_456
   * }}}
   */
  case class Int(value: scala.Int) extends AstExpression

  /**
   * {{{
   *   "hello world\n"
   * }}}
   */
  case class String(value: java.lang.String) extends AstExpression

  /**
   * {{{
   *   f{ 1+1=2 }
   * }}}
   */
  case class DlExpression(value: org.keymaerax.core.Expression) extends AstExpression

  /**
   * {{{
   *   s{ ==> 1+1=2 }
   * }}}
   */
  case class DlSequent(value: org.keymaerax.core.Sequent) extends AstExpression

  /**
   * {{{
   *   #not
   * }}}
   */
  case class BuiltinFunction(value: org.keymaerax.hippolang.BuiltinFunction) extends AstExpression

  /**
   * {{{
   *   import <path>
   * }}}
   */
  case class Import(path: AstExpression, slice: SourceFile#Slice) extends AstExpression

  /**
   * {{{
   * val <name> = <value>
   * var <name> = <value>
   * export val <name> = <value>
   * export var <name> = <value>
   * }}}
   */
  case class Declare(exports: Boolean, mutable: Boolean, name: AstIdentifier, value: AstExpression)
      extends AstExpression

  /**
   * {{{
   * <name> = <value>
   * }}}
   */
  case class Assign(name: AstIdentifier, value: AstExpression) extends AstExpression

  /**
   * {{{
   * <name>
   * }}}
   */
  case class Lookup(name: AstIdentifier) extends AstExpression

  /**
   * {{{
   *   if (<condition>) <ifTrue>
   *   if (<condition>) <ifTrue> else <ifFalse>
   * }}}
   */
  case class If(condition: AstExpression.Parens, ifTrue: AstExpression, ifFalse: Option[AstExpression])
      extends AstExpression

  /**
   * {{{
   *   while (<condition>) <body>
   * }}}
   */
  case class While(condition: AstExpression.Parens, body: AstExpression) extends AstExpression

  /**
   * {{{
   *   function(a, b) a + b
   * }}}
   */
  case class Function(args: Seq[AstIdentifier], body: AstExpression) extends AstExpression

  /**
   * {{{
   *   theorem <conclusion>
   *   premise <premise> ...
   *   by <proof>
   * }}}
   */
  case class Theorem(verified: Boolean, conclusion: AstExpression, premises: Seq[AstExpression], proof: AstExpression)
      extends AstExpression

  /**
   * {{{
   *   (<expr>; <expr>;)
   *   (<expr>; <expr>; <returnExpr>)
   * }}}
   */
  case class Parens(exprs: Seq[AstExpression], returnExpr: Option[AstExpression]) extends AstExpression

  /**
   * {{{
   *   {<expr>; <expr>;}
   *   {<expr>; <expr>; <returnExpr>}
   * }}}
   */
  case class Block(exprs: Seq[AstExpression], returnExpr: Option[AstExpression]) extends AstExpression

  /**
   * {{{
   *   backward { ... }
   * }}}
   */
  case class BackwardBlock(inner: Block) extends AstExpression

  /**
   * {{{
   *   graph { ... }
   * }}}
   */
  case class GraphBlock(inner: Block) extends AstExpression

  ////////////
  // Suffix //
  ////////////

  /**
   * {{{
   *   <target>.#<member>
   * }}}
   */
  case class BuiltinAccess(target: AstExpression, member: org.keymaerax.hippolang.BuiltinMemberFunction)
      extends AstExpression

  /**
   * {{{
   *   <target>.<name>
   * }}}
   */
  case class Access(target: AstExpression, name: AstIdentifier) extends AstExpression

  /**
   * {{{
   *   <target>(<args>)
   * }}}
   */
  case class Apply(target: AstExpression, args: IndexedSeq[AstExpression]) extends AstExpression

  /**
   * {{{
   *   <target>[<args>]
   * }}}
   */
  case class ApplyTactic(target: AstExpression, args: IndexedSeq[AstExpression]) extends AstExpression

  ////////////
  // Prefix //
  ////////////

  /**
   * {{{
   *   !<target>
   * }}}
   */
  case class Not(target: AstExpression) extends AstExpression

  /**
   * {{{
   *   -<target>
   * }}}
   */
  case class Neg(target: AstExpression) extends AstExpression

  ///////////
  // Infix //
  ///////////

  /**
   * {{{
   *   <left> * <right>
   * }}}
   */
  case class Mul(left: AstExpression, right: AstExpression) extends AstExpression

  /**
   * {{{
   *   <left> / <right>
   * }}}
   */
  case class Div(left: AstExpression, right: AstExpression) extends AstExpression

  /**
   * {{{
   *   <left> + <right>
   * }}}
   */
  case class Add(left: AstExpression, right: AstExpression) extends AstExpression

  /**
   * {{{
   *   <left> - <right>
   * }}}
   */
  case class Sub(left: AstExpression, right: AstExpression) extends AstExpression

  /**
   * {{{
   *   <left> > <right>
   * }}}
   */
  case class Gt(left: AstExpression, right: AstExpression) extends AstExpression

  /**
   * {{{
   *   <left> >= <right>
   * }}}
   */
  case class Gte(left: AstExpression, right: AstExpression) extends AstExpression

  /**
   * {{{
   *   <left> < <right>
   * }}}
   */
  case class Lt(left: AstExpression, right: AstExpression) extends AstExpression

  /**
   * {{{
   *   <left> <= <right>
   * }}}
   */
  case class Lte(left: AstExpression, right: AstExpression) extends AstExpression

  /**
   * {{{
   *   <left> == <right>
   * }}}
   */
  case class Eq(left: AstExpression, right: AstExpression) extends AstExpression

  /**
   * {{{
   *   <left> != <right>
   * }}}
   */
  case class Neq(left: AstExpression, right: AstExpression) extends AstExpression

  /**
   * {{{
   *   <left> && <right>
   * }}}
   */
  case class And(left: AstExpression, right: AstExpression) extends AstExpression

  /**
   * {{{
   *   <left> || <right>
   * }}}
   */
  case class Or(left: AstExpression, right: AstExpression) extends AstExpression

  /**
   * {{{
   *   <left> -> <right>
   * }}}
   */
  case class MapsTo(left: AstExpression, right: AstExpression) extends AstExpression
}
