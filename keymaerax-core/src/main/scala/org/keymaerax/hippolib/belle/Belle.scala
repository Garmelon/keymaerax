/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.belle

import org.keymaerax.bellerophon.{
  BelleExpr,
  BelleInterpreter,
  BelleProvable,
  Fixed,
  PositionLocator,
  ReflectiveExpressionBuilder,
}
import org.keymaerax.btactics.macros.{
  ArgInfo,
  DerivationInfo,
  ExpressionArg,
  FormulaArg,
  GeneratorArg,
  ListArg,
  NumberArg,
  OptionArg,
  PosInExprArg,
  StringArg,
  SubstitutionArg,
  TermArg,
  VariableArg,
}
import org.keymaerax.core.*
import org.keymaerax.hippocore.definitions.Definitions
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippocore.{BackwardTactic, HippoException}
import org.keymaerax.infrastruct.{PosInExpr, Position}
import org.keymaerax.parser.Declaration
import org.keymaerax.pt.ElidingProvable

case class Belle(name: String, args: BelleValue*) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(name).digestSeq(args).hash

  override def runBackward(
      ctx: HippoContext,
      conclusion: HippoSequent,
      premises: Map[Int, HippoSequent],
  ): HippoProof = {
    HippoException.require(DerivationInfo.hasCodeName(name), s"No bellerophon tactic named $name exists")
    val info = DerivationInfo.ofCodeName(name)

    val belleExpr = ReflectiveExpressionBuilder(
      name = name,
      arguments = Belle.asArgs(args, info.numPositionArgs, info.persistentInputs),
      generator = None,
      defs = Declaration(Map.empty),
    )

    val extraDefs = args.map(Belle.collectDefs).fold(Definitions.empty)(_.merge(_))
    Belle.runBelleExpr(ctx, conclusion, belleExpr, extraDefs)
  }
}

object Belle {

  /** @see [[org.keymaerax.bellerophon.parser.DLBelleParser.positionLocator]] */
  private def asPositionArg(value: BelleValue): PositionLocator = value match {
    case BelleValue.Int(v) => Fixed(Position(v))
    case BelleValue.Seq(v) =>
      HippoException.require(v.nonEmpty, "Position argument must not be an empty list")
      val ints = v.collect { case BelleValue.Int(v) => v }
      HippoException.require(v.length == ints.length, "Position arguments must all be integers")
      val pos :: posInExpr = v.map(_.asInstanceOf[Int]).toList
      Fixed(Position(pos, posInExpr))
    case _ => HippoException.fail("Position argument must be an integer or a list of integers")
  }

  /**
   * Convert an [[Any]] into a value that [[ReflectiveExpressionBuilder]] will understand for the given [[ArgInfo]].
   *
   * Since Hippolang does not support every core type, multiple conversions from other types are included so every type
   * of argument can still be written in Hippolang.
   *
   * In the future, Bellerophon arguments can hopefully be refactored to get rid of the weird List wrapped around
   * everything, but for now, this function tries its best to integrate with Bellerophon's weird system.
   *
   * @see
   *   [[ReflectiveExpressionBuilder.build]]
   * @see
   *   [[org.keymaerax.bellerophon.parser.DLBelleParser.argumentInterior]]
   */
  private def asArg(value: BelleValue, info: ArgInfo): Seq[Any] = info match {
    case _: FormulaArg => List(value match {
        case BelleValue.Expression(v: Formula, _) => v
        case _ => HippoException.fail("Argument must be a Formula")
      })

    case _: NumberArg => List(value match {
        case BelleValue.Expression(v: Number, _) => v
        case BelleValue.Int(v) => Number(v)
        case _ => HippoException.fail("Argument must be a Number or an integer")
      })

    case _: VariableArg => List(value match {
        case BelleValue.Expression(v: Variable, _) => v
        case BelleValue.String(v) =>
          // See DLParser.variable
          val regex = "^(?<name>[a-zA-Z][a-zA-Z0-9]*_*)(?:_(?<index>0|[1-9][0-9]*))?(?<diff>')?$".r
          val matched = regex.findFirstMatchIn(v) match {
            case None => HippoException.fail("Variable identifier has invalid format")
            case Some(matched) => matched
          }
          val name = matched.group("name")
          val index = Option(matched.group("index")).map(_.toInt)
          val differential = matched.group("diff") != null
          val variable = Variable(name = name, index = index)
          if (differential) DifferentialSymbol(variable) else variable
        case _ => HippoException.fail("Argument must be a Variable or a string")
      })

    case _: TermArg => List(value match {
        case BelleValue.Expression(v: Term, _) => v
        case _ => HippoException.fail("Argument must be a Term")
      })

    case _: ExpressionArg => List(value match {
        case BelleValue.Expression(v, _) => v
        case _ => HippoException.fail("Argument must be an Expression")
      })

    case _: SubstitutionArg => List(value match {
        case BelleValue.Substitution(v) => v
        case BelleValue.Seq(Seq(BelleValue.Expression(what, _), BelleValue.Expression(repl, _))) =>
          SubstitutionPair(what, repl)
        case _ => HippoException.fail("Argument must be a SubstitutionPair or an Expression list of length 2")
      })

    case _: PosInExprArg => List(value match {
        case BelleValue.PosInExpr(v) => v
        case BelleValue.Seq(v) =>
          val ints = v.collect { case BelleValue.Int(v) => v }
          HippoException.require(v.length == ints.length, "List must contain only integers")
          PosInExpr(ints.toList)
        case _ => HippoException.fail("Argument must be a PosInExpr or a list of integers")
      })

    case _: GeneratorArg => HippoException.fail("Generator arguments are not supported")

    case _: StringArg => List(value match {
        case BelleValue.String(v) => v
        case _ => HippoException.fail("Argument must be a string")
      })

    case OptionArg(inner) => value match {
        case BelleValue.Option(None) => List()
        case BelleValue.Option(Some(v)) => asArg(v, inner)
        case v => asArg(v, inner)
      }

    case ListArg(inner) => value match {
        // This flatMap seems incorrect in the case of nested lists, but it's what DLBelleParser does.
        // ReflectiveExpressionBuilder only allows very specific lists, which never contain nested lists,
        // so our behavior in those cases doesn't really matter... for now.
        // Hopefully Bellerophon's "an argument is always a list" shtick will be refactored at some point,
        // then this logic would become quite a bit nicer too.
        case BelleValue.Seq(v) => v.flatMap(asArg(_, inner))
        case _ => HippoException.fail("Argument must be a list")
      }
  }

  private def asArgs(
      values: Seq[BelleValue],
      numPositionArgs: Int,
      argInfos: Seq[ArgInfo],
  ): List[Either[Seq[Any], PositionLocator]] = {
    val (positionValues, nonPositionValues) = values.splitAt(numPositionArgs)

    HippoException.require(
      positionValues.length == numPositionArgs,
      s"Too few arguments, at least $numPositionArgs arguments required",
    )

    HippoException.require(
      nonPositionValues.length <= argInfos.length,
      s"Too many arguments, at most ${numPositionArgs + argInfos.length} allowed",
    )

    val positionArgs = positionValues.map(asPositionArg)
    val nonPositionArgs = nonPositionValues.zip(argInfos).map(asArg.tupled)
    (positionArgs.map(Right.apply) ++ nonPositionArgs.map(Left.apply)).toList
  }

  def collectDefs(value: BelleValue): Definitions = value match {
    case BelleValue.Expression(_, defs) => defs
    case BelleValue.Seq(value) => value.map(collectDefs).fold(Definitions.empty)(_.merge(_))
    case _ => Definitions.empty
  }

  def runBelleExpr(
      ctx: HippoContext,
      conclusion: HippoSequent,
      belleExpr: BelleExpr,
      extraDefs: Definitions = Definitions.empty,
  ): HippoProof = {
    val startProvable = Provable.startProof(conclusion.sequent)
    val startProvableSig = ElidingProvable(startProvable, Declaration(Map.empty))

    val resultValue = BelleInterpreter(belleExpr, BelleProvable(startProvableSig, None))
    val resultProvable = resultValue match {
      case BelleProvable(p, _) => p.underlyingProvable
      case _ => HippoException.fail("Bellerophon interpreter did not return a Provable")
    }

    ctx.belle(resultProvable, conclusion.defs.merge(extraDefs))
  }
}
