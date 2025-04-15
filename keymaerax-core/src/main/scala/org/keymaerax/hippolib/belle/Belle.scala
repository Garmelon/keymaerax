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
import org.keymaerax.core.{DifferentialSymbol, Provable, Sequent, SubstitutionPair, Variable}
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippocore.{BackwardTactic, HippoException}
import org.keymaerax.hippolang.HippoValue
import org.keymaerax.infrastruct.{PosInExpr, Position}
import org.keymaerax.parser.Declaration
import org.keymaerax.pt.ElidingProvable

case class Belle(name: String, args: Seq[HippoValue]) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(name).digestSeq(args).hash

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = {
    HippoException.require(DerivationInfo.hasCodeName(name), s"No bellerophon tactic named $name exists")
    val info = DerivationInfo.ofCodeName(name)

    val belleExpr = ReflectiveExpressionBuilder(
      name = name,
      arguments = Belle.asArgs(args, info.numPositionArgs, info.persistentInputs),
      generator = None,
      defs = Declaration(Map.empty),
    )

    Belle.runBelleExpr(ctx, conclusion, belleExpr)
  }
}

object Belle {

  /** @see [[org.keymaerax.bellerophon.parser.DLBelleParser.positionLocator]] */
  private def asPositionArg(value: HippoValue): PositionLocator = value match {
    case HippoValue.Int(value) => Fixed(Position(value))

    case HippoValue.List(values) =>
      if (values.isEmpty) ???
      val pos :: posInExpr = values.map(_.asInt).toList
      Fixed(Position(pos) ++ PosInExpr(posInExpr))

    case _ => HippoException.fail("Position argument must be an integer or a list of integerss")
  }

  /** @see [[org.keymaerax.bellerophon.parser.DLBelleParser.argumentInterior]] */
  private def asArg(value: HippoValue, info: ArgInfo): Seq[Any] = info match {
    case _: FormulaArg => List(value.asExpression)
    case _: TermArg => List(value.asExpression)
    case _: ExpressionArg => List(value.asExpression)

    case _: VariableArg =>
      // See DLParser.variable
      val regex = "^(?<name>[a-zA-Z][a-zA-Z0-9]*_*)(?:_(?<index>0|[1-9][0-9]*))?(?<diff>')?$".r
      val matched = regex.findFirstMatchIn(value.asString) match {
        case None => HippoException.fail("Variable identifier has invalid format")
        case Some(matched) => matched
      }
      val name = matched.group("name")
      val index = Option(matched.group("index")).map(_.toInt)
      val differential = matched.group("diff") != null
      val variable = Variable(name = name, index = index)
      List(if (differential) DifferentialSymbol(variable) else variable)

    case _: GeneratorArg => HippoException.fail("Generator arguments are not supported")

    case _: StringArg => List(value.asString)

    case _: SubstitutionArg =>
      val (whatValue, replValue) = value match {
        case HippoValue.List(Seq(whatValue, replValue)) => (whatValue, replValue)
        case _ => HippoException.fail("Argument must be a list of length 2")
      }
      List(SubstitutionPair(what = whatValue.asExpression, repl = replValue.asExpression))

    case _: PosInExprArg => List(value match {
        case HippoValue.Null => PosInExpr()
        case HippoValue.Int(value) => PosInExpr(List(value))
        case HippoValue.List(values) => PosInExpr(values.map(_.asInt).toList)
        case _ => HippoException.fail("Argument must be null, an integer, or a list of integers")
      })

    case OptionArg(inner) => value match {
        case HippoValue.Null => List()
        case value => asArg(value, inner)
      }

    case ListArg(inner) => value match {
        // This flatMap seems incorrect in the case of nested lists, but it's what DLBelleParser does.
        // It seems that nested ListArg-s don't usually happen "in the wild".
        // If at some point Bellerophon's whole "an argument is actually always a list" business is refactored,
        // this logic could become quite a bit nicer and less hacky as well.
        case HippoValue.List(values) => values.flatMap(asArg(_, inner))
        case _ => HippoException.fail("Argument must be a list")
      }

    case _: NumberArg => List(value.asInt)
  }

  private def asArgs(
      values: Seq[HippoValue],
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

  def runBelleExpr(ctx: HippoContext, conclusion: Sequent, belleExpr: BelleExpr): HippoProof = {
    val startProvable = Provable.startProof(conclusion)
    val startProvableSig = ElidingProvable(startProvable, Declaration(Map.empty))

    val resultValue = BelleInterpreter(belleExpr, BelleProvable(startProvableSig, None))
    val resultProvable = resultValue match {
      case BelleProvable(p, _) => p.underlyingProvable
      case _ => HippoException.fail("Bellerophon interpreter did not return a Provable")
    }

    ctx.belle(resultProvable)
  }
}
