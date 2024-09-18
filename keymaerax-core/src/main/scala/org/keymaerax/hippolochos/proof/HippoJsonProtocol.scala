/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.proof

import org.keymaerax.core.SubstitutionPair
import org.keymaerax.hippolochos.tools.Hash
import org.keymaerax.parser.FullPrettyPrinter
import org.keymaerax.{core, GlobalState}
import spray.json._

object HippoJsonProtocol extends DefaultJsonProtocol {
  val discriminant = "type"
  private val printer = FullPrettyPrinter
  private val parser = GlobalState.parser

  def variant(discriminant: String, fields: JsField*): JsObject =
    JsObject((this.discriminant -> discriminant.toJson) +: fields: _*)

  private def variantO(discriminant: String, o: JsValue): JsObject =
    JsObject(o.asJsObject.fields + (this.discriminant -> discriminant.toJson))

  ////////////////
  // Core types //
  ////////////////

  implicit object FormulaFormat extends JsonFormat[core.Formula] {
    override def write(obj: core.Formula): JsValue = JsString(printer(obj))
    override def read(json: JsValue): core.Formula = parser.formulaParser(json.convertTo[String])
  }

  implicit object ExpressionFormat extends JsonFormat[core.Expression] {
    override def write(obj: core.Expression): JsValue = JsString(printer(obj))
    override def read(json: JsValue): core.Expression = parser(json.convertTo[String])
  }

  implicit object SequentFormat extends JsonFormat[core.Sequent] {
    override def write(obj: core.Sequent): JsValue = JsString(printer(obj))
    override def read(json: JsValue): core.Sequent = parser.sequentParser(json.convertTo[String])
  }

  // Sort

  implicit val objectSortFormat: RootJsonFormat[core.ObjectSort] = jsonFormat(core.ObjectSort, "name")

  implicit object SortFormat extends JsonFormat[core.Sort] {
    override def write(obj: core.Sort): JsValue = obj match {
      case core.Unit => variant("unit")
      case core.Bool => variant("bool")
      case core.Real => variant("real")
      case core.Trafo => variant("trafo")
      case core.Tuple(left, right) => variant("tuple", "left" -> write(left), "right" -> write(right))
      case o: core.ObjectSort => variantO("objectSort", o.toJson)
    }

    override def read(json: JsValue): core.Sort = {
      val obj = json.asJsObject
      obj.fields(discriminant).convertTo[String] match {
        case "unit" => core.Unit
        case "bool" => core.Bool
        case "real" => core.Real
        case "trafo" => core.Trafo
        case "tuple" => core.Tuple(left = read(obj.fields("left")), right = read(obj.fields("right")))
        case "objectSort" => json.convertTo[core.ObjectSort]
        case _ => deserializationError("Sort expected")
      }
    }
  }

  // Variable

  implicit val baseVariableFormat: RootJsonFormat[core.BaseVariable] =
    jsonFormat(core.BaseVariable, "name", "index", "sort")

  implicit object VariableFormat extends RootJsonFormat[core.Variable] {
    override def write(obj: core.Variable): JsValue = obj match {
      case o: core.BaseVariable => variantO("base", o.toJson)
      case core.DifferentialSymbol(x) => variant("diff", "x" -> write(x))
    }

    override def read(json: JsValue): core.Variable = {
      val obj = json.asJsObject
      obj.fields(discriminant).convertTo[String] match {
        case "base" => obj.convertTo[core.BaseVariable]
        case "diff" => core.DifferentialSymbol(x = read(obj.fields("x")))
        case _ => deserializationError("Variable expected")
      }
    }
  }

  // URename and USubst

  implicit val uRenameFormat: RootJsonFormat[core.URename] = jsonFormat(core.URename, "what", "repl", "semantic")

  implicit val substitutionPairFormat: RootJsonFormat[core.SubstitutionPair] =
    jsonFormat(core.SubstitutionPair, "what", "repl")

  implicit object USubstFormat extends JsonFormat[core.USubst] {
    override def write(obj: core.USubst): JsValue = obj.subsDefsInput.toJson
    override def read(json: JsValue): core.USubst = core.USubst(json.convertTo[Seq[SubstitutionPair]])
  }

  // SeqPos

  implicit object SeqPosFormat extends JsonFormat[core.SeqPos] {
    override def write(obj: core.SeqPos): JsValue = obj.getPos.toJson
    override def read(json: JsValue): core.SeqPos = json.convertTo[Int] match {
      case n if n < 0 => core.AntePos(-n - 1)
      case n if n > 0 => core.SuccPos(n - 1)
      case _ => deserializationError("SeqPos expected")
    }
  }

  implicit object AntePosFormat extends JsonFormat[core.AntePos] {
    override def write(obj: core.AntePos): JsValue = SeqPosFormat.write(obj)
    override def read(json: JsValue): core.AntePos = json.convertTo[core.SeqPos] match {
      case pos: core.AntePos => pos
      case _: core.SuccPos => deserializationError("AntePos expected")
    }
  }

  implicit object SuccPosFormat extends JsonFormat[core.SuccPos] {
    override def write(obj: core.SuccPos): JsValue = SeqPosFormat.write(obj)
    override def read(json: JsValue): core.SuccPos = json.convertTo[core.SeqPos] match {
      case pos: core.SuccPos => pos
      case _: core.AntePos => deserializationError("SuccPos expected")
    }
  }

  // Rule

  implicit val hideRightFormat: RootJsonFormat[core.HideRight] = jsonFormat(core.HideRight, "pos")
  implicit val hideLeftFormat: RootJsonFormat[core.HideLeft] = jsonFormat(core.HideLeft, "pos")
  implicit val exchangeRightRuleFormat: RootJsonFormat[core.ExchangeRightRule] =
    jsonFormat(core.ExchangeRightRule, "pos1", "pos2")
  implicit val exchangeLeftRuleFormat: RootJsonFormat[core.ExchangeLeftRule] =
    jsonFormat(core.ExchangeLeftRule, "pos1", "pos2")
  implicit val closeFormat: RootJsonFormat[core.Close] = jsonFormat(core.Close, "assume", "pos")
  implicit val closeTrueFormat: RootJsonFormat[core.CloseTrue] = jsonFormat(core.CloseTrue, "pos")
  implicit val closeFalseFormat: RootJsonFormat[core.CloseFalse] = jsonFormat(core.CloseFalse, "pos")
  implicit val cutFormat: RootJsonFormat[core.Cut] = jsonFormat(core.Cut, "c")
  implicit val notRightFormat: RootJsonFormat[core.NotRight] = jsonFormat(core.NotRight, "pos")
  implicit val notLeftFormat: RootJsonFormat[core.NotLeft] = jsonFormat(core.NotLeft, "pos")
  implicit val andRightFormat: RootJsonFormat[core.AndRight] = jsonFormat(core.AndRight, "pos")
  implicit val andLeftFormat: RootJsonFormat[core.AndLeft] = jsonFormat(core.AndLeft, "pos")
  implicit val orRightFormat: RootJsonFormat[core.OrRight] = jsonFormat(core.OrRight, "pos")
  implicit val orLeftFormat: RootJsonFormat[core.OrLeft] = jsonFormat(core.OrLeft, "pos")
  implicit val implyRightFormat: RootJsonFormat[core.ImplyRight] = jsonFormat(core.ImplyRight, "pos")
  implicit val implyLeftFormat: RootJsonFormat[core.ImplyLeft] = jsonFormat(core.ImplyLeft, "pos")
  implicit val equivRightFormat: RootJsonFormat[core.EquivRight] = jsonFormat(core.EquivRight, "pos")
  implicit val equivLeftFormat: RootJsonFormat[core.EquivLeft] = jsonFormat(core.EquivLeft, "pos")
  implicit val uniformRenamingFormat: RootJsonFormat[core.UniformRenaming] =
    jsonFormat(core.UniformRenaming.apply, "what", "repl")
  implicit val boundRenamingFormat: RootJsonFormat[core.BoundRenaming] =
    jsonFormat(core.BoundRenaming, "what", "repl", "pos")
  implicit val skolemizeFormat: RootJsonFormat[core.Skolemize] = jsonFormat(core.Skolemize, "pos")
  implicit val coHideRightFormat: RootJsonFormat[core.CoHideRight] = jsonFormat(core.CoHideRight, "pos")
  implicit val coHideLeftFormat: RootJsonFormat[core.CoHideLeft] = jsonFormat(core.CoHideLeft, "pos")
  implicit val coHide2Format: RootJsonFormat[core.CoHide2] = jsonFormat(core.CoHide2, "pos1", "pos2")
  implicit val cutRightFormat: RootJsonFormat[core.CutRight] = jsonFormat(core.CutRight, "c", "pos")
  implicit val cutLeftFormat: RootJsonFormat[core.CutLeft] = jsonFormat(core.CutLeft, "c", "pos")
  implicit val commuteEquivRightFormat: RootJsonFormat[core.CommuteEquivRight] =
    jsonFormat(core.CommuteEquivRight, "pos")
  implicit val commuteEquivLeftFormat: RootJsonFormat[core.CommuteEquivLeft] = jsonFormat(core.CommuteEquivLeft, "pos")
  implicit val equivifyRightFormat: RootJsonFormat[core.EquivifyRight] = jsonFormat(core.EquivifyRight, "pos")

  implicit object RuleFormat extends RootJsonFormat[core.Rule] {
    override def write(obj: core.Rule): JsValue = obj match {
      case o: core.HideRight => variantO("hideRight", o.toJson)
      case o: core.HideLeft => variantO("hideLeft", o.toJson)
      case o: core.ExchangeRightRule => variantO("exchangeRightRule", o.toJson)
      case o: core.ExchangeLeftRule => variantO("exchangeLeftRule", o.toJson)
      case o: core.Close => variantO("close", o.toJson)
      case o: core.CloseTrue => variantO("closeTrue", o.toJson)
      case o: core.CloseFalse => variantO("closeFalse", o.toJson)
      case o: core.Cut => variantO("cut", o.toJson)
      case o: core.NotRight => variantO("notRight", o.toJson)
      case o: core.NotLeft => variantO("notLeft", o.toJson)
      case o: core.AndRight => variantO("andRight", o.toJson)
      case o: core.AndLeft => variantO("andLeft", o.toJson)
      case o: core.OrRight => variantO("orRight", o.toJson)
      case o: core.OrLeft => variantO("orLeft", o.toJson)
      case o: core.ImplyRight => variantO("implyRight", o.toJson)
      case o: core.ImplyLeft => variantO("implyLeft", o.toJson)
      case o: core.EquivRight => variantO("equivRight", o.toJson)
      case o: core.EquivLeft => variantO("equivLeft", o.toJson)
      case o: core.UniformRenaming => variantO("uniformRenaming", o.toJson)
      case o: core.BoundRenaming => variantO("boundRenaming", o.toJson)
      case o: core.Skolemize => variantO("skolemize", o.toJson)
      case o: core.CoHideRight => variantO("coHideRight", o.toJson)
      case o: core.CoHideLeft => variantO("coHideLeft", o.toJson)
      case o: core.CoHide2 => variantO("coHide2", o.toJson)
      case o: core.CutRight => variantO("cutRight", o.toJson)
      case o: core.CutLeft => variantO("cutLeft", o.toJson)
      case o: core.CommuteEquivRight => variantO("commuteEquivRight", o.toJson)
      case o: core.CommuteEquivLeft => variantO("commuteEquivLeft", o.toJson)
      case o: core.EquivifyRight => variantO("equivifyRight", o.toJson)
    }

    override def read(json: JsValue): core.Rule = json.asJsObject.fields(discriminant).convertTo[String] match {
      case "hideRight" => json.convertTo[core.HideRight]
      case "hideLeft" => json.convertTo[core.HideLeft]
      case "exchangeRightRule" => json.convertTo[core.ExchangeRightRule]
      case "exchangeLeftRule" => json.convertTo[core.ExchangeLeftRule]
      case "close" => json.convertTo[core.Close]
      case "closeTrue" => json.convertTo[core.CloseTrue]
      case "closeFalse" => json.convertTo[core.CloseFalse]
      case "cut" => json.convertTo[core.Cut]
      case "notRight" => json.convertTo[core.NotRight]
      case "notLeft" => json.convertTo[core.NotLeft]
      case "andRight" => json.convertTo[core.AndRight]
      case "andLeft" => json.convertTo[core.AndLeft]
      case "orRight" => json.convertTo[core.OrRight]
      case "orLeft" => json.convertTo[core.OrLeft]
      case "implyRight" => json.convertTo[core.ImplyRight]
      case "implyLeft" => json.convertTo[core.ImplyLeft]
      case "equivRight" => json.convertTo[core.EquivRight]
      case "equivLeft" => json.convertTo[core.EquivLeft]
      case "uniformRenaming" => json.convertTo[core.UniformRenaming]
      case "boundRenaming" => json.convertTo[core.BoundRenaming]
      case "skolemize" => json.convertTo[core.Skolemize]
      case "coHideRight" => json.convertTo[core.CoHideRight]
      case "coHideLeft" => json.convertTo[core.CoHideLeft]
      case "coHide2" => json.convertTo[core.CoHide2]
      case "cutRight" => json.convertTo[core.CutRight]
      case "cutLeft" => json.convertTo[core.CutLeft]
      case "commuteEquivRight" => json.convertTo[core.CommuteEquivRight]
      case "commuteEquivLeft" => json.convertTo[core.CommuteEquivLeft]
      case "equivifyRight" => json.convertTo[core.EquivifyRight]
      case _ => deserializationError("Rule expected")
    }
  }

  /////////////////
  // Hippo types //
  /////////////////

  implicit object HashFormat extends JsonFormat[Hash] {
    override def write(obj: Hash): JsValue = JsString(obj.hexString)
    override def read(json: JsValue): Hash = Hash(json.convertTo[String])
  }

  implicit val externalSourceQeToolFormat: RootJsonFormat[ExternalSource.QeTool] =
    jsonFormat(ExternalSource.QeTool, "formula")

  implicit val externalSourceDerivedFormat: RootJsonFormat[ExternalSource.Derived] =
    jsonFormat(ExternalSource.Derived, "hash")

  implicit object ExternalSourceFormat extends RootJsonFormat[ExternalSource] {
    override def write(obj: ExternalSource): JsValue = obj match {
      case ExternalSource.Sorry => variant("sorry")
      case o: ExternalSource.QeTool => variantO("qeTool", o.toJson)
      case o: ExternalSource.Derived => variantO("derived", o.toJson)
    }

    override def read(json: JsValue): ExternalSource = json.asJsObject.fields(discriminant).convertTo[String] match {
      case "sorry" => ExternalSource.Sorry
      case "qeTool" => json.convertTo[ExternalSource.QeTool]
      case "derived" => json.convertTo[ExternalSource.Derived]
      case _ => deserializationError("ExternalSource expected")
    }
  }

  implicit val hippoPremiseFormat: RootJsonFormat[HippoPremise] =
    jsonFormat(HippoPremise.apply, "sequent", "mustBeProved")

}
