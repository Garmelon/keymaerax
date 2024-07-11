/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.parse

import fastparse.ScalaWhitespace._
import fastparse._
import org.keymaerax.hippolochus.{HippoBuiltin, HippoIdentifier}
import org.keymaerax.parser.DLParser

object HippoParser {
  // When the parser is changed, this alphabetically sorted list of keywords must be kept up-to-date.
  // To parse a keyword, always use the corresponding constant instead of a magic string value.
  // This helps ensure the list does not become outdated.
  private val keywordBy = "by"
  private val keywordDgl = "dGL"
  private val keywordDone = "done"
  private val keywordFalse = "false"
  private val keywordFork = "fork"
  private val keywordNull = "null"
  private val keywordOn = "on"
  private val keywordTheorem = "theorem"
  private val keywordTrue = "true"
  val keywords: Set[String] = Set(
    keywordBy,
    keywordDgl,
    keywordDone,
    keywordFalse,
    keywordFork,
    keywordNull,
    keywordOn,
    keywordTheorem,
    keywordTrue,
  )

  // This dlParser instance does not depend on global state.
  private val dlParser = new DLParser(checkAgainst = None)

  def parse(source: String): Parsed[AstExpression] = fastparse.parse(source, program(_), verboseFailures = true)

  /**
   * An identifier consists of one or more characters from the set `a-zA-Z0-9_`. The first character must not be a
   * digit, to prevent confusion with integer literals.
   *
   * @see
   *   [[HippoIdentifier]], [[intLiteral]]
   */
  def identifier[$: P]: P[AstIdentifier] = P({
    def startChar = CharPred(HippoIdentifier.isValidStartChar(_))
    def restChars = CharsWhile(HippoIdentifier.isValidChar(_)).?
    def identifier = (startChar ~~ restChars).!.map(HippoIdentifier(_))
    def quotedIdentifier = "'" ~~ identifier ~~ "'"
    (quotedIdentifier | identifier).map(AstIdentifier)
  }).opaque("identifier")

  //////////////
  // Literals //
  //////////////

  def nullLiteral[$: P]: P[AstLiteral.Null] = P(HippoParser.keywordNull.!.map(_ => AstLiteral.Null()))

  /** A boolean literal is either `true` or `false`. */
  def boolLiteral[$: P]: P[AstLiteral.Bool] = P(
    HippoParser.keywordTrue.!.map(_ => AstLiteral.Bool(true)) |
      HippoParser.keywordFalse.!.map(_ => AstLiteral.Bool(false))
  )

  /**
   * An integer literal is an optional sign (`+` or `-`), followed directly with no spaces by one or more decimal
   * digits. Digits may be separated and followed by an arbitrary amount of underscores `_`, which can aid readability.
   *
   * The digit part of an integer literal must start with a digit. `_3` or `-_42` are not valid integer literals, but
   * `3_` and `-42_` are. This is to prevent parsing ambiguities and confusion with identifiers. For example. `_3` is a
   * valid identifier.
   *
   * @see
   *   [[identifier]], [[negExpression]]
   */
  def intLiteral[$: P]: P[AstLiteral.Int] = P({
    def sign = ("+" | "-").?
    def body = CharIn("0-9") ~~ CharsWhileIn("0-9_").?
    def value = (sign ~~ body).!.map(_.replaceAll("_", "")).map(Integer.parseInt)
    value.map(AstLiteral.Int)
  })

  def stringLiteral[$: P]: P[AstLiteral.String] = P({
    def unescapedChars = CharsWhile(c => !(c == '\\' || c == '"')).!
    def escapedChar = "\\\\".!.map(_ => "\\") | "\\\"".!.map(_ => "\"")
    ("\"" ~~ (unescapedChars | escapedChar).repX ~~ "\"").map(_.mkString).map(AstLiteral.String)
  })

  def dglLiteral[$: P]: P[AstLiteral.Dgl] =
    P((HippoParser.keywordDgl ~ "{" ~ dlParser.formula ~ "}").map(AstLiteral.Dgl))

  def builtinLiteral[$: P]: P[AstLiteral.Builtin] = P(
    ("#" ~~ identifier)
      .flatMapX(name => HippoBuiltin.byName.get(name.name).map(Pass(_)).getOrElse(Fail))
      .map(AstLiteral.Builtin)
  )

  def literal[$: P]: P[AstLiteral] =
    P(nullLiteral | boolLiteral | intLiteral | stringLiteral | dglLiteral | builtinLiteral)

  ///////////////////////////
  // Primitive expressions //
  ///////////////////////////

  def literalExpression[$: P]: P[AstExpression.Literal] = P(literal.map(literal => AstExpression.Literal(literal)))

  def theoremExpression[$: P]: P[AstExpression.Theorem] = P(
    (HippoParser.keywordTheorem ~ expression ~ HippoParser.keywordBy ~ blockExpression).map { case (statement, proof) =>
      AstExpression.Theorem(statement, proof)
    }
  )

  def onExpression[$: P]: P[AstExpression.On] =
    P((HippoParser.keywordOn ~ literal ~ blockExpression).map { case (label, proof) => AstExpression.On(label, proof) })

  def forkExpression[$: P]: P[AstExpression.Fork] =
    P((HippoParser.keywordFork ~ "{" ~ (onExpression ~ ";").rep ~ "}").map(AstExpression.Fork))

  def doneExpression[$: P]: P[AstExpression.Done] = P(HippoParser.keywordDone.!.map(_ => AstExpression.Done()))

  def lookupExpression[$: P]: P[AstExpression.Lookup] = P(identifier.map(name => AstExpression.Lookup(name)))

  def assignExpression[$: P]: P[AstExpression.Assign] = P((identifier ~ "="./ ~ expression).map { case (name, value) =>
    AstExpression.Assign(name, value)
  })

  def parensExpression[$: P]: P[AstExpression.Parens] =
    P(("(" ~ (expression ~ ";").rep ~ ")").map(AstExpression.Parens))

  def blockExpression[$: P]: P[AstExpression.Block] = P(("{" ~ (expression ~ ";").rep ~ "}").map(AstExpression.Block))

  def primitiveExpression[$: P]: P[AstExpression] = P(
    literalExpression | theoremExpression | onExpression | forkExpression | doneExpression | assignExpression |
      lookupExpression | parensExpression | blockExpression
  )

  ////////////////////////
  // Atomic expressions //
  ////////////////////////

  // An atomic expression is a primitive expression with suffixes and prefixes, for example negation.

  def accessExpression[$: P]: P[AstExpression => AstExpression.Access] =
    P(("." ~ identifier).map(name => inner => AstExpression.Access(target = inner, name = name)))

  def applyExpression[$: P]: P[AstExpression => AstExpression.Apply] = P(
    ("(" ~ expression.rep(sep = ",") ~ ",".? ~ ")")
      .map(args => inner => AstExpression.Apply(target = inner, args = args.toIndexedSeq))
  )

  def suffixExpression[$: P]: P[AstExpression => AstExpression] = P(accessExpression | applyExpression)

  def notExpression[$: P]: P[AstExpression => AstExpression.Not] = P("!".!.map(_ => AstExpression.Not))

  /**
   * Negate an expression via prefixed `-`.
   *
   * We have to be a bit careful as a `-` in front of an integer literal belongs to said literal: `-3` must be parsed as
   * a single negative literal, not a negation of a positive literal. Since integer literals don't allow spaces between
   * the sign and the digits, `- 3` however must be parsed like `-(3)`, i.e. the negation of a positive integer literal.
   *
   * @see
   *   [[intLiteral]]
   */
  def negExpression[$: P]: P[AstExpression => AstExpression.Neg] =
    P(("-" ~~ !CharIn("0-9")).!.map(_ => AstExpression.Neg))

  def prefixExpression[$: P]: P[AstExpression => AstExpression] = P(notExpression | negExpression)

  def atomicExpression[$: P]: P[AstExpression] = P(
    (prefixExpression.rep ~ primitiveExpression ~ suffixExpression.rep).map { case (prefixes, atom, suffixes) =>
      val suffixed = suffixes.foldLeft(atom)((inner, suffix) => suffix(inner))
      prefixes.foldRight(suffixed)((prefix, inner) => prefix(inner))
    }
  )

  ///////////////////////////
  // Composite expressions //
  ///////////////////////////

  type InfixOpConstructor = (AstExpression, AstExpression) => AstExpression

  def infixOp[$: P](symbol: String, constructor: InfixOpConstructor): P[InfixOpConstructor] =
    P(symbol.!.map(_ => constructor))

  def leftAssociativeInfixOpExpression[$: P](
      atom: => P[AstExpression],
      op: => P[InfixOpConstructor],
  ): P[AstExpression] = P((atom ~ (op./ ~ atom).rep).map { case (atom, ops) =>
    ops.foldLeft(atom) { case (left, (op, right)) => op(left, right) }
  })

  def expression[$: P]: P[AstExpression] = P(leftAssociativeInfixOpExpression(
    leftAssociativeInfixOpExpression(
      leftAssociativeInfixOpExpression(
        leftAssociativeInfixOpExpression(
          leftAssociativeInfixOpExpression(
            leftAssociativeInfixOpExpression(
              atomicExpression,
              infixOp("*", AstExpression.Mul) | infixOp("/", AstExpression.Div),
            ),
            infixOp("+", AstExpression.Add) | infixOp("-", AstExpression.Sub),
          ),
          infixOp(">=", AstExpression.Gte) | infixOp(">", AstExpression.Gt) | infixOp("<=", AstExpression.Lte) |
            infixOp("<", AstExpression.Lt),
        ),
        infixOp("==", AstExpression.Eq) | infixOp("!=", AstExpression.Neq),
      ),
      infixOp("&&", AstExpression.And),
    ),
    infixOp("||", AstExpression.Or),
  ))

  def program[$: P]: P[AstExpression.Block] = P((Start ~ (expression ~ ";").rep ~ End).map(AstExpression.Block))
}
