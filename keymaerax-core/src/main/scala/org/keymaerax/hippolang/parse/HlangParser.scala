/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.parse

import fastparse.*
import fastparse.ScalaWhitespace.*
import org.keymaerax.hippolang.parse.HlangParser.*
import org.keymaerax.hippolang.{HlangException, HlangIdentifier}
import org.keymaerax.parser.DLParser

class HlangParser(source: SourceFile) {
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
  private def slice[$: P](inner: => P[_]): P[source.Slice] =
    P { (Index ~~ inner.! ~~ Index).map { case (start, _, end) => source.Slice(start, end) } }

  // https://com-lihaoyi.github.io/fastparse/#HigherOrderParsers
  private def sliced[$: P, T](inner: => P[T]): P[(T, source.Slice)] =
    P { (Index ~~ inner ~~ Index).map { case (start, value, end) => (value, source.Slice(start, end)) } }

  /**
   * An identifier consists of one or more characters from the set `a-zA-Z0-9_`. The first character must not be a
   * digit, to prevent confusion with integer literals.
   *
   * @see
   *   [[HlangIdentifier]], [[intExpression]]
   */
  private def identifier[$: P]: P[AstIdentifier] = P {
    def startChar = CharPred(HlangIdentifier.isValidStartChar(_))
    def restChars = CharsWhile(HlangIdentifier.isValidChar(_)).?
    def identifier = (startChar ~~ restChars).!.map(HlangIdentifier.apply)
    def quotedIdentifier = "'" ~~ identifier ~~ "'"
    (quotedIdentifier | identifier).map(AstIdentifier.apply)
  }.opaque("identifier")

  private def goalIdentifier[$: P]: P[AstIdentifier] = P { "$" ~ identifier }

  private def singleArgumentList[$: P]: P[AstIdentifier] = "(" ~/ identifier ~ ",".? ~ ")"

  private def argumentList[$: P]: P[Seq[AstIdentifier]] = "(" ~/ identifier.rep(sep = ",") ~ ",".? ~ ")"

  private def tacticArgumentList[$: P]: P[Seq[AstIdentifier]] = "[" ~/ identifier.rep(sep = ",") ~ ",".? ~ "]"

  private def tacticGoalArgumentList[$: P]: P[Seq[AstIdentifier]] = "[" ~/ goalIdentifier.rep(sep = ",") ~ ",".? ~ "]"

  ///////////////////////////
  // Primitive expressions //
  ///////////////////////////

  private def nullExpression[$: P]: P[AstExpression.Null] =
    P { slice(keywordNull).map(slice => AstExpression.Null(slice = slice)) }

  /** A boolean literal is either `true` or `false`. */
  private def boolExpression[$: P]: P[AstExpression.Bool] = P {
    def literal = keywordTrue.!.map(_ => true) | keywordFalse.!.map(_ => false)
    sliced(literal).map { case (value, slice) => AstExpression.Bool(slice = slice, value = value) }
  }

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
  private def intExpression[$: P]: P[AstExpression.Int] = P {
    def sign = ("+" | "-").?
    def body = CharIn("0-9") ~~/ CharsWhileIn("0-9_").?
    def value = (sign ~~ body).!.map(_.replaceAll("_", "")).map(Integer.parseInt)
    sliced(value).map { case (value, slice) => AstExpression.Int(slice = slice, value = value) }
  }.opaque("integer")

  private def stringExpression[$: P]: P[AstExpression.String] = P {
    def unescapedChars = CharsWhile(c => !(c == '\\' || c == '"')).!
    def escapedChar = "\\" ~~ SingleChar.map {
      case 'b' => '\b'
      case 'f' => '\f'
      case 'n' => '\n'
      case 'r' => '\r'
      case 't' => '\t'
      case c => c
    }
    sliced("\"" ~~/ (unescapedChars | escapedChar).repX ~~ "\"").map { case (segments, slice) =>
      AstExpression.String(slice = slice, value = segments.mkString)
    }
  }.opaque("string")

  private def dlExpressionExpression[$: P]: P[AstExpression] = P {
    def raw = "!".!.?.map(_.isDefined)

    def dlTerm = sliced(keywordDlTerm ~/ raw ~ argumentList.? ~ "{" ~ dlParser.term(true) ~ "}")
      .map { case ((raw, args, value), slice) =>
        AstExpression.DlTerm(slice = slice, raw = raw, args = args, value = value)
      }

    def dlFormula = sliced(keywordDlFormula ~/ raw ~ argumentList.? ~ "{" ~ dlParser.formula ~ "}")
      .map { case ((raw, args, value), slice) =>
        AstExpression.DlFormula(slice = slice, raw = raw, args = args, value = value)
      }

    def dlFormulaPredicational = sliced(
      keywordDlFormulaPredicational ~/ raw ~ singleArgumentList.? ~ "{" ~ dlParser.formula ~ "}"
    ).map { case ((raw, arg, value), slice) =>
      AstExpression.DlFormulaPredicational(slice = slice, raw = raw, arg = arg, value = value)
    }

    def dlProgram = sliced(keywordDlProgram ~/ raw ~ "{" ~ dlParser.program ~ "}").map { case ((raw, value), slice) =>
      AstExpression.DlProgram(slice = slice, raw = raw, value = value)
    }
    dlTerm | dlFormulaPredicational | dlFormula | dlProgram
  }

  private def dlSequentExpression[$: P]: P[AstExpression.DlSequent] = P {
    def raw = "!".!.?.map(_.isDefined)

    sliced(keywordDlSequent ~/ raw ~ "{" ~ dlParser.sequent ~ "}").map { case ((raw, value), slice) =>
      AstExpression.DlSequent(slice = slice, raw = raw, value = value)
    }
  }

  private def importExpression[$: P]: P[AstExpression.Import] = P {
    sliced(keywordImport ~/ expression).map { case (path, slice) => AstExpression.Import(path = path, slice = slice) }
  }

  private def declareExpression[$: P]: P[AstExpression.Declare] = P {
    def mutable = keywordVal.!.map(_ => false) | keywordVar.!.map(_ => true)
    sliced(slice(keywordExport./).? ~ mutable ~/ identifier ~ "=" ~ expression).map {
      case ((exportSlice, mutable, name, value), slice) => AstExpression
          .Declare(slice = slice, exportSlice = exportSlice, mutable = mutable, name = name, value = value)
    }
  }

  private def assignExpression[$: P]: P[AstExpression.Assign] = P {
    sliced(identifier ~ "=" ~ expression).map { case ((name, value), slice) =>
      AstExpression.Assign(slice = slice, name = name, value = value)
    }
  }

  private def assignGoalExpression[$: P]: P[AstExpression.AssignGoal] = P {
    sliced(sliced(goalIdentifier) ~ ":=" ~ expression).map { case ((name, nameSlice, value), slice) =>
      AstExpression.AssignGoal(slice = slice, nameSlice = nameSlice, name = name, value = value)
    }
  }

  private def lookupExpression[$: P]: P[AstExpression.Lookup] =
    P { sliced(identifier).map { case (name, slice) => AstExpression.Lookup(slice = slice, name = name) } }

  private def lookupGoalExpression[$: P]: P[AstExpression.LookupGoal] =
    P { sliced(goalIdentifier).map { case (name, slice) => AstExpression.LookupGoal(slice = slice, name = name) } }

  private def ifExpression[$: P]: P[AstExpression.If] = P {
    sliced(keywordIf ~/ parensExpression ~ expression ~ (keywordElse ~/ expression).?)
      .map { case ((condition, ifTrue, ifFalse), slice) =>
        AstExpression.If(slice = slice, condition = condition, ifTrue = ifTrue, ifFalse = ifFalse)
      }
  }

  private def whileExpression[$: P]: P[AstExpression.While] = P {
    sliced(keywordWhile ~/ parensExpression ~ expression).map { case ((condition, body), slice) =>
      AstExpression.While(slice = slice, condition = condition, body = body)
    }
  }

  private def matchExpression[$: P]: P[AstExpression.Match] = P {
    sliced(
      keywordMatch ~/ parensExpression ~ "{" ~
        (keywordCase ~ (dlExpressionExpression | dlSequentExpression) ~ "=>" ~ expression ~ ",").rep ~
        (keywordOtherwise ~ "=>" ~ expression ~ ",").? ~ "}"
    ).map { case ((target, cases, otherwise), slice) =>
      AstExpression.Match(slice = slice, target = target, cases = cases, otherwise = otherwise)
    }
  }

  private def functionExpression[$: P]: P[AstExpression.Function] = P {
    sliced(keywordFunction ~/ argumentList ~/ expression).map { case ((args, body), slice) =>
      AstExpression.Function(slice = slice, args = args, body = body)
    }
  }

  private def theoremExpression[$: P]: P[AstExpression.Theorem] = P {
    sliced(
      slice(keywordVerified./).? ~ keywordTheorem ~/ expression ~ (keywordGiven ~/ expression)
        .rep ~ keywordBy ~/ sliced(expression)
    ).map { case ((verifySlice, conclusion, premises, (proof, proofSlice)), slice) =>
      AstExpression.Theorem(
        slice = slice,
        verifySlice = verifySlice,
        conclusion = conclusion,
        premises = premises,
        proof = proof,
        proofSlice = proofSlice,
      )
    }
  }

  private def parensExpression[$: P]: P[AstExpression.Parens] = P {
    sliced("(" ~/ (NoCut(expression) ~ ";"./).rep ~ expression.? ~ ")").map { case ((exprs, returnExpr), slice) =>
      AstExpression.Parens(slice = slice, exprs = exprs, returnExpr = returnExpr)
    }
  }

  private def blockExpression[$: P]: P[AstExpression.Block] = P {
    sliced("{" ~/ (NoCut(expression) ~ ";"./).rep ~ expression.? ~ "}").map { case ((exprs, returnExpr), slice) =>
      AstExpression.Block(slice = slice, exprs = exprs, returnExpr = returnExpr)
    }
  }

  private def forwardExpression[$: P]: P[AstExpression.ForwardBlock] = P {
    sliced(keywordForward ~/ tacticArgumentList.? ~ blockExpression).map { case ((premises, inner), slice) =>
      AstExpression.ForwardBlock(slice = slice, premises = premises.getOrElse(Seq.empty), inner = inner)
    }
  }

  private def backwardExpression[$: P]: P[AstExpression] = P {
    // Combines backward blocks and backward assignment so we can cut on the "backward" keyword.
    type Block = (AstIdentifier, AstExpression.Block)
    type Assign = (source.Slice, AstExpression)
    def block: P[Either[Block, Assign]] = ("->" ~ goalIdentifier ~ blockExpression).map(Left.apply)
    def assign: P[Either[Block, Assign]] = (slice(":=") ~ expression).map(Right.apply)
    sliced(keywordBackward ~/ tacticGoalArgumentList.? ~ (block | assign)).map {
      case ((premises, Left((conclusion, inner))), slice) => AstExpression.BackwardBlock(
          slice = slice,
          premises = premises.getOrElse(Seq.empty),
          conclusion = conclusion,
          inner = inner,
        )
      case ((premises, Right((assignSlice, value))), slice) => AstExpression.BackwardAssign(
          slice = slice,
          assignSlice = assignSlice,
          premises = premises.getOrElse(Seq.empty),
          value = value,
        )
    }
  }

  private def graphBlockExpression[$: P]: P[AstExpression.GraphBlock] = P {
    sliced(keywordGraph ~/ blockExpression).map { case (inner, slice) =>
      AstExpression.GraphBlock(slice = slice, inner = inner)
    }
  }

  private def primitiveExpression[$: P]: P[AstExpression] = P {
    nullExpression | boolExpression | intExpression | stringExpression | dlSequentExpression | dlExpressionExpression |
      importExpression | declareExpression | ifExpression | whileExpression | matchExpression | functionExpression |
      theoremExpression | parensExpression | blockExpression | forwardExpression | backwardExpression |
      graphBlockExpression |
      // Assignment must come before lookup because lookup is a prefix of assignment.
      assignGoalExpression | lookupGoalExpression |
      // Because these two start with a literal, they have to come last so they don't shadow literals like "while".
      // Otherwise, "while (foo) ..." is interpreted as an apply on the literal "while",
      // and due to cuts, results in a parse error because it expects a ";" to follow.
      assignExpression | lookupExpression
  }

  ////////////////////////
  // Atomic expressions //
  ////////////////////////

  // An atomic expression is a primitive expression with suffixes and prefixes, for example negation.

  private type SuffixOpConstructor = (SourceFile#Slice, AstExpression) => AstExpression

  private def accessExpression[$: P]: P[SuffixOpConstructor] = P {
    ("." ~/ sliced(identifier)).map { case (name, nameSlice) =>
      (slice, inner) => AstExpression.Access(slice = slice, nameSlice = nameSlice, target = inner, name = name)
    }
  }

  private def applyExpression[$: P]: P[SuffixOpConstructor] = P {
    sliced("(" ~/ expression.rep(sep = ",") ~ ",".? ~ ")").map { case (args, argsSlice) =>
      (slice, inner) =>
        AstExpression.Apply(slice = slice, target = inner, args = args.toIndexedSeq, argsSlice = argsSlice)
    }
  }

  private def applyTacticExpression[$: P]: P[SuffixOpConstructor] = P {
    sliced("[" ~/ expression.rep(sep = ",") ~ ",".? ~ "]").map { case (args, argsSlice) =>
      (slice, inner) =>
        AstExpression.ApplyTactic(slice = slice, target = inner, args = args.toIndexedSeq, argsSlice = argsSlice)
    }
  }

  private def pipeTacticExpression[$: P]: P[SuffixOpConstructor] = P {
    (slice(":<") ~/ expression).map { case (opSlice, arg) =>
      (slice, inner) => AstExpression.PipeLeftTactic(slice = slice, opSlice = opSlice, target = inner, arg = arg)
    }
  }

  private def suffixExpression[$: P]: P[SuffixOpConstructor] =
    P { accessExpression | applyExpression | applyTacticExpression | pipeTacticExpression }

  private type PrefixOpConstructor = (SourceFile#Slice, AstExpression) => AstExpression

  private def notExpression[$: P]: P[PrefixOpConstructor] = P {
    slice("!").map(opSlice => (slice, target) => AstExpression.Not(slice = slice, opSlice = opSlice, target = target))
  }

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
  private def negExpression[$: P]: P[PrefixOpConstructor] = P {
    slice("-" ~~ !CharIn("0-9"))
      .map(opSlice => (slice, target) => AstExpression.Neg(slice = slice, opSlice = opSlice, target = target))
  }

  private def spreadExpression[$: P]: P[PrefixOpConstructor] =
    P { slice("..").map(_ => (slice, target) => AstExpression.Spread(slice = slice, target = target)) }

  private def prefixExpression[$: P]: P[PrefixOpConstructor] = P { notExpression | negExpression | spreadExpression }

  private def atomicExpression[$: P]: P[AstExpression] = P {
    (sliced(prefixExpression).rep ~ primitiveExpression ~ sliced(suffixExpression).rep)
      .map { case (prefixes, atom, suffixes) =>
        val suffixed = suffixes.foldLeft(atom) { case (inner, (suffix, suffixSlice)) =>
          suffix(source.Slice(inner.slice.start, suffixSlice.end), inner)
        }
        prefixes.foldRight(suffixed) { case ((prefix, prefixSlice), inner) =>
          prefix(source.Slice(prefixSlice.start, inner.slice.end), inner)
        }
      }
  }

  ///////////////////////////
  // Composite expressions //
  ///////////////////////////

  private def infixOp[$: P](
      symbol: String,
      constructor: (SourceFile#Slice, SourceFile#Slice, AstExpression, AstExpression) => AstExpression,
  ): P[(SourceFile#Slice, AstExpression, AstExpression) => AstExpression] =
    P { slice(symbol).map(opSlice => (slice, left, right) => constructor(slice, opSlice, left, right)) }

  private def leftAssociativeInfixOpExpression[$: P](
      atom: => P[AstExpression],
      op: => P[(SourceFile#Slice, AstExpression, AstExpression) => AstExpression],
  ): P[AstExpression] = P {
    (atom ~ (op./ ~ atom).rep).map { case (atom, ops) =>
      val start = atom.slice.start
      var end = atom.slice.end
      var result = atom

      for ((op, right) <- ops) {
        end = right.slice.end
        result = op(source.Slice(start, end), result, right)
      }

      result
    }
  }

  private def leftAssociativeInfixOpExpressions[$: P](
      atom: () => P[AstExpression],
      ops: (() => P[(SourceFile#Slice, AstExpression, AstExpression) => AstExpression])*
  ): P[AstExpression] =
    P { ops.foldLeft(atom) { case (atom, op) => () => leftAssociativeInfixOpExpression(atom(), op()) }() }

  private def expression[$: P]: P[AstExpression] = P {
    leftAssociativeInfixOpExpressions(
      () => atomicExpression,
      () => infixOp("*", AstExpression.Mul.apply) | infixOp("/", AstExpression.Div.apply),
      () => infixOp("+", AstExpression.Add.apply) | infixOp("-", AstExpression.Sub.apply),
      () =>
        infixOp(">=", AstExpression.Gte.apply) | infixOp(">", AstExpression.Gt.apply) |
          infixOp("<=", AstExpression.Lte.apply) | infixOp("<", AstExpression.Lt.apply),
      () => infixOp("==", AstExpression.Eq.apply) | infixOp("!=", AstExpression.Neq.apply),
      () => infixOp("&&", AstExpression.And.apply),
      () => infixOp("||", AstExpression.Or.apply),
      () => infixOp("->", AstExpression.MapsTo.apply),
      () =>
        infixOp(
          ":>",
          (slice, opSlice, lhs, rhs) =>
            AstExpression
              .ApplyTactic(slice = slice, target = rhs, args = IndexedSeq(lhs), argsSlice = opSlice + lhs.slice),
        ),
    )
  }

  private def program[$: P]: P[AstExpression.Block] = P {
    sliced(Start ~ (expression ~ ";"./).rep ~ End).map { case (exprs, slice) =>
      AstExpression.Block(slice = slice, exprs = exprs, returnExpr = None)
    }
  }
}

object HlangParser {
  // When the parser is changed, this alphabetically sorted list of keywords must be kept up-to-date.
  // To parse a keyword, always use the corresponding constant instead of a magic string value.
  // This helps ensure the list does not become outdated.
  private val keywordBackward = "backward"
  private val keywordBy = "by"
  private val keywordCase = "case"
  private val keywordDlFormula = "dLf"
  private val keywordDlFormulaPredicational = "dLfp"
  private val keywordDlProgram = "dLp"
  private val keywordDlSequent = "dLs"
  private val keywordDlTerm = "dLt"
  private val keywordElse = "else"
  private val keywordExport = "export"
  private val keywordFalse = "false"
  private val keywordForward = "forward"
  private val keywordFunction = "function"
  private val keywordGiven = "given"
  private val keywordGraph = "graph"
  private val keywordIf = "if"
  private val keywordImport = "import"
  private val keywordMatch = "match"
  private val keywordNull = "null"
  private val keywordOtherwise = "otherwise"
  private val keywordTheorem = "theorem"
  private val keywordTrue = "true"
  private val keywordVal = "val"
  private val keywordVar = "var"
  private val keywordVerified = "verified"
  private val keywordWhile = "while"
  val keywords: Set[String] = Set(
    keywordBackward,
    keywordBy,
    keywordCase,
    keywordDlFormula,
    keywordDlFormulaPredicational,
    keywordDlProgram,
    keywordDlSequent,
    keywordDlTerm,
    keywordElse,
    keywordExport,
    keywordFalse,
    keywordForward,
    keywordFunction,
    keywordGiven,
    keywordGraph,
    keywordIf,
    keywordImport,
    keywordMatch,
    keywordNull,
    keywordOtherwise,
    keywordTheorem,
    keywordTrue,
    keywordVal,
    keywordVar,
    keywordVerified,
    keywordWhile,
  )
}
