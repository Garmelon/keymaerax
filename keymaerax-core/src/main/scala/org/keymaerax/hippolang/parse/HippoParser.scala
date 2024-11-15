/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.parse

import fastparse.ScalaWhitespace._
import fastparse._
import org.keymaerax.hippolang.parse.HippoParser._
import org.keymaerax.hippolang.{BuiltinFunction, BuiltinMemberFunction, HippoIdentifier, HlangException}
import org.keymaerax.parser.DLParser

class HippoParser(source: SourceFile) {
  // This dlParser instance does not depend on global state.
  private val dlParser = new DLParser

  def parse(): AstExpression = fastparse.parse[AstExpression](source.text, program(_), verboseFailures = true) match {
    case success: Parsed.Success[AstExpression] => success.value
    case failure: Parsed.Failure => throw HlangException(
        message = "Parsing failed",
        label = s"expected ${failure.label}",
        slice = source.Slice(failure.index),
      )
  }

  // https://com-lihaoyi.github.io/fastparse/#HigherOrderParsers
  private def slice[$: P](inner: => P[_]): P[source.Slice] = P((Index ~~ inner.! ~~ Index).map { case (start, _, end) =>
    source.Slice(start, end)
  })

  // https://com-lihaoyi.github.io/fastparse/#HigherOrderParsers
  private def sliced[$: P, T](inner: => P[T]): P[(T, source.Slice)] =
    P((Index ~~ inner ~~ Index).map { case (start, value, end) => (value, source.Slice(start, end)) })

  /**
   * An identifier consists of one or more characters from the set `a-zA-Z0-9_`. The first character must not be a
   * digit, to prevent confusion with integer literals.
   *
   * @see
   *   [[HippoIdentifier]], [[intExpression]]
   */
  private def identifier[$: P]: P[AstIdentifier] = P({
    def startChar = CharPred(HippoIdentifier.isValidStartChar(_))
    def restChars = CharsWhile(HippoIdentifier.isValidChar(_)).?
    def identifier = (startChar ~~ restChars).!.map(HippoIdentifier(_))
    def quotedIdentifier = "'" ~~ identifier ~~ "'"
    (quotedIdentifier | identifier).map(AstIdentifier)
  }).opaque("identifier")

  ///////////////////////////
  // Primitive expressions //
  ///////////////////////////

  private def nullExpression[$: P]: P[AstExpression.Null] = P(keywordNull.!.map(_ => AstExpression.Null()))

  /** A boolean literal is either `true` or `false`. */
  private def boolExpression[$: P]: P[AstExpression.Bool] =
    P(keywordTrue.!.map(_ => AstExpression.Bool(true)) | keywordFalse.!.map(_ => AstExpression.Bool(false)))

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
  private def intExpression[$: P]: P[AstExpression.Int] = P({
    def sign = ("+" | "-").?
    def body = CharIn("0-9") ~~/ CharsWhileIn("0-9_").?
    def value = (sign ~~ body).!.map(_.replaceAll("_", "")).map(Integer.parseInt)
    value.map(AstExpression.Int)
  }).opaque("integer")

  private def stringExpression[$: P]: P[AstExpression.String] = P({
    def unescapedChars = CharsWhile(c => !(c == '\\' || c == '"')).!
    def escapedChar = "\\" ~~ SingleChar.map {
      case 'b' => '\b'
      case 'f' => '\f'
      case 'n' => '\n'
      case 'r' => '\r'
      case 't' => '\t'
      case c => c
    }
    ("\"" ~~/ (unescapedChars | escapedChar).repX ~~ "\"").map(_.mkString).map(AstExpression.String)
  }).opaque("string")

  private def dlExpressionExpression[$: P]: P[AstExpression.DlExpression] =
    P((keywordDlExpression ~/ "{" ~ dlParser.expression ~ "}").map(AstExpression.DlExpression))

  private def dlSequentExpression[$: P]: P[AstExpression.DlSequent] =
    P((keywordDlSequent ~/ "{" ~ dlParser.sequent ~ "}").map(AstExpression.DlSequent))

  private def builtinFunctionExpression[$: P]: P[AstExpression.BuiltinFunction] = P(
    ("#" ~~/ identifier)
      .flatMapX(name => BuiltinFunction.byName.get(name.name).map(Pass(_)).getOrElse(Fail))
      .map(foo => AstExpression.BuiltinFunction(foo))
  ).opaque("builtin function")

  private def importExpression[$: P]: P[AstExpression.Import] = P(sliced(keywordImport ~/ expression).map {
    case (path, slice) => AstExpression.Import(path = path, slice = slice)
  })

  private def declareExpression[$: P]: P[AstExpression.Declare] = P({
    def const = keywordVal.!.map(_ => false) | keywordVar.!.map(_ => true)
    (slice(keywordExport./).? ~ const ~/ identifier ~ "=" ~ expression).map {
      case (exportSlice, mutable, name, value) => AstExpression.Declare(exportSlice = exportSlice, mutable, name, value)
    }
  })

  private def assignExpression[$: P]: P[AstExpression.Assign] =
    P((identifier ~ "=" ~/ expression).map { case (name, value) => AstExpression.Assign(name, value) })

  private def lookupExpression[$: P]: P[AstExpression.Lookup] = P(identifier.map(AstExpression.Lookup))

  private def ifExpression[$: P]: P[AstExpression.If] = P(
    (keywordIf ~/ parensExpression ~ expression ~ (keywordElse ~/ expression).?)
      .map { case (condition, ifTrue, ifFalse) => AstExpression.If(condition, ifTrue, ifFalse) }
  )

  private def whileExpression[$: P]: P[AstExpression.While] = P((keywordWhile ~/ parensExpression ~ expression).map {
    case (condition, body) => AstExpression.While(condition, body)
  })

  private def functionExpression[$: P]: P[AstExpression.Function] = P(
    (keywordFunction ~/ "(" ~ identifier.rep(sep = ","./) ~ ",".? ~ ")" ~/ expression).map { case (args, body) =>
      AstExpression.Function(args, body)
    }
  )

  private def theoremExpression[$: P]: P[AstExpression.Theorem] = P({
    def verified = keywordVerified./.!.?.map(_.isDefined)
    (verified ~ keywordTheorem ~/ expression ~ (keywordPremise ~/ expression).rep ~ keywordBy ~/ expression).map {
      case (verified, statement, premises, proof) => AstExpression.Theorem(verified, statement, premises, proof)
    }
  })

  private def parensExpression[$: P]: P[AstExpression.Parens] = P(
    ("(" ~/ (NoCut(expression) ~ ";"./).rep ~ expression.? ~ ")").map { case (exprs, returnExpr) =>
      AstExpression.Parens(exprs, returnExpr)
    }
  )

  private def blockExpression[$: P]: P[AstExpression.Block] = P(
    ("{" ~/ (NoCut(expression) ~ ";"./).rep ~ expression.? ~ "}").map { case (exprs, returnExpr) =>
      AstExpression.Block(exprs, returnExpr)
    }
  )

  private def backwardBlockExpression[$: P]: P[AstExpression.BackwardBlock] =
    P((keywordBackward ~/ blockExpression).map(AstExpression.BackwardBlock))

  private def graphBlockExpression[$: P]: P[AstExpression.GraphBlock] =
    P((keywordGraph ~/ blockExpression).map(AstExpression.GraphBlock))

  private def primitiveExpression[$: P]: P[AstExpression] = P(
    // dlSequentExpression must come before dlExpressionExpression since "dL" is a prefix of "dLs".
    nullExpression | boolExpression | intExpression | stringExpression | dlSequentExpression | dlExpressionExpression |
      builtinFunctionExpression | importExpression | declareExpression | ifExpression | whileExpression |
      functionExpression | theoremExpression | parensExpression | blockExpression | backwardBlockExpression |
      graphBlockExpression |
      // Because these two start with a literal, they have to come last so they don't shadow literals like "while".
      // Otherwise, "while (foo) ..." is interpreted as an apply on the literal "while",
      // and due to cuts, results in a parse error because it expects a ";" to follow.
      assignExpression | lookupExpression
  )

  ////////////////////////
  // Atomic expressions //
  ////////////////////////

  // An atomic expression is a primitive expression with suffixes and prefixes, for example negation.

  private def builtinAccessExpression[$: P]: P[AstExpression => AstExpression.BuiltinAccess] = P(
    (".#" ~/ identifier)
      .flatMapX(name => BuiltinMemberFunction.byName.get(name.name).map(Pass(_)).getOrElse(Fail))
      .map(builtin => (inner: AstExpression) => AstExpression.BuiltinAccess(target = inner, member = builtin))
  ).opaque("builtin member function")

  private def accessExpression[$: P]: P[AstExpression => AstExpression.Access] =
    P(("." ~/ identifier).map(name => inner => AstExpression.Access(target = inner, name = name)))

  private def applyExpression[$: P]: P[AstExpression => AstExpression.Apply] = P(
    ("(" ~/ expression.rep(sep = ","./) ~ ",".? ~ ")")
      .map(args => inner => AstExpression.Apply(target = inner, args = args.toIndexedSeq))
  )

  private def applyTacticExpression[$: P]: P[AstExpression => AstExpression.ApplyTactic] = P(
    ("[" ~/ expression.rep(sep = ","./) ~ ",".? ~ "]")
      .map(args => inner => AstExpression.ApplyTactic(target = inner, args = args.toIndexedSeq))
  )

  private def suffixExpression[$: P]: P[AstExpression => AstExpression] =
    P(builtinAccessExpression | accessExpression | applyExpression | applyTacticExpression)

  private def notExpression[$: P]: P[AstExpression => AstExpression.Not] = P("!".!.map(_ => AstExpression.Not))

  /**
   * Negate an expression via prefixed `-`.
   *
   * We have to be a bit careful as a `-` in front of an integer literal belongs to said literal: `-3` must be parsed as
   * a single negative literal, not a negation of a positive literal. Since integer literals don't allow spaces between
   * the sign and the digits, `- 3` however must be parsed like `-(3)`, i.e. the negation of a positive integer literal.
   *
   * @see
   *   [[intExpression]]
   */
  private def negExpression[$: P]: P[AstExpression => AstExpression.Neg] =
    P(("-" ~~ !CharIn("0-9")).!.map(_ => AstExpression.Neg))

  private def prefixExpression[$: P]: P[AstExpression => AstExpression] = P(notExpression | negExpression)

  private def atomicExpression[$: P]: P[AstExpression] = P(
    (prefixExpression.rep ~ primitiveExpression ~ suffixExpression.rep).map { case (prefixes, atom, suffixes) =>
      val suffixed = suffixes.foldLeft(atom)((inner, suffix) => suffix(inner))
      prefixes.foldRight(suffixed)((prefix, inner) => prefix(inner))
    }
  )

  ///////////////////////////
  // Composite expressions //
  ///////////////////////////

  private type InfixOpConstructor = (AstExpression, AstExpression) => AstExpression

  private def infixOp[$: P](symbol: String, constructor: InfixOpConstructor): P[InfixOpConstructor] =
    P(symbol.!.map(_ => constructor))

  private def leftAssociativeInfixOpExpression[$: P](
      atom: => P[AstExpression],
      op: => P[InfixOpConstructor],
  ): P[AstExpression] = P((atom ~ (op./ ~ atom).rep).map { case (atom, ops) =>
    ops.foldLeft(atom) { case (left, (op, right)) => op(left, right) }
  })

  private def expression[$: P]: P[AstExpression] = P(leftAssociativeInfixOpExpression(
    leftAssociativeInfixOpExpression(
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
    ),
    infixOp("->", AstExpression.MapsTo),
  ))

  private def program[$: P]: P[AstExpression.Block] =
    P((Start ~ (expression ~ ";"./).rep ~ End).map(AstExpression.Block(_, returnExpr = None)))
}

object HippoParser {
  // When the parser is changed, this alphabetically sorted list of keywords must be kept up-to-date.
  // To parse a keyword, always use the corresponding constant instead of a magic string value.
  // This helps ensure the list does not become outdated.
  private val keywordBackward = "backward"
  private val keywordBy = "by"
  private val keywordDlExpression = "dL"
  private val keywordDlSequent = "dLs"
  private val keywordElse = "else"
  private val keywordExport = "export"
  private val keywordFalse = "false"
  private val keywordFunction = "function"
  private val keywordGraph = "graph"
  private val keywordIf = "if"
  private val keywordImport = "import"
  private val keywordNull = "null"
  private val keywordPremise = "premise"
  private val keywordTheorem = "theorem"
  private val keywordTrue = "true"
  private val keywordVal = "val"
  private val keywordVar = "var"
  private val keywordVerified = "verified"
  private val keywordWhile = "while"
  val keywords: Set[String] = Set(
    keywordBackward,
    keywordBy,
    keywordDlExpression,
    keywordDlSequent,
    keywordElse,
    keywordExport,
    keywordFalse,
    keywordFunction,
    keywordGraph,
    keywordIf,
    keywordImport,
    keywordNull,
    keywordPremise,
    keywordTheorem,
    keywordTrue,
    keywordVal,
    keywordVar,
    keywordVerified,
    keywordWhile,
  )
}
