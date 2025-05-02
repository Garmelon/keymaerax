/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.proof

import org.keymaerax.core.{Rule, Sequent, URename, USubst}
import org.keymaerax.hippocore.proof.HippoJsonProtocol.*
import spray.json.*

import java.util
import scala.collection.mutable

object HippoJson {
  private def addProof(
      jsProofs: mutable.Buffer[JsValue],
      indexByProof: util.IdentityHashMap[HippoProof, Int],
      proof: HippoProof,
  ): Int = {
    for (index <- Option(indexByProof.get(proof))) return index

    val jsProof = proof match {
      case HippoProof.External(conclusion, premises, source) =>
        variant("external", "conclusion" -> conclusion.toJson, "premises" -> premises.toJson, "source" -> source.toJson)
      case HippoProof.Sequent(conclusion) => variant("sequent", "conclusion" -> conclusion.toJson)
      case HippoProof.CoreAxiom(name) => variant("coreAxiom", "name" -> name.toJson)
      case HippoProof.CoreAxiomaticRule(name) => variant("coreAxiomaticRule", "name" -> name.toJson)
      case HippoProof.CoreProofRule(conclusion, rule) =>
        variant("coreProofRule", "conclusion" -> conclusion.toJson, "rule" -> rule.toJson)
      case HippoProof.URename(proof, rename) =>
        variant("uRename", "proof" -> addProof(jsProofs, indexByProof, proof).toJson, "rename" -> rename.toJson)
      case HippoProof.USubst(proof, subst) =>
        variant("uSubst", "proof" -> addProof(jsProofs, indexByProof, proof).toJson, "subst" -> subst.toJson)
      case HippoProof.GloballySoundUSubst(premise, subst) =>
        variant("globallySoundUSubst", "premise" -> premise.toJson, "subst" -> subst.toJson)
      case HippoProof.Join(proof, subproof, at) => variant(
          "join",
          "proof" -> addProof(jsProofs, indexByProof, proof).toJson,
          "subproof" -> addProof(jsProofs, indexByProof, subproof).toJson,
          "at" -> at.toJson,
        )
      case HippoProof.Swap(proof, premise1, premise2) => variant(
          "swap",
          "proof" -> addProof(jsProofs, indexByProof, proof).toJson,
          "premise1" -> premise1.toJson,
          "premise2" -> premise2.toJson,
        )
      case HippoProof.Deduplicate(proof, premise, duplicate) => variant(
          "deduplicate",
          "proof" -> addProof(jsProofs, indexByProof, proof).toJson,
          "premise" -> premise.toJson,
          "duplicate" -> duplicate.toJson,
        )
      case HippoProof.Weaken(proof, premise) =>
        variant("weaken", "proof" -> addProof(jsProofs, indexByProof, proof).toJson, "premise" -> premise.toJson)

    }

    val index = jsProofs.length
    indexByProof.put(proof, index)
    jsProofs.append(jsProof)
    index
  }

  private def addJsProof(proofs: mutable.IndexedBuffer[HippoProof], jsProof: JsValue): Unit = {
    val fields = jsProof.asJsObject.fields
    val proof = fields(discriminant).convertTo[String] match {
      case "external" => HippoProof.External(
          conclusion = fields("conclusion").convertTo[HippoSequent],
          premises = fields("premises").convertTo[IndexedSeq[HippoPremise]],
          source = fields("source").convertTo[ExternalSource],
        )
      case "sequent" => HippoProof.Sequent(conclusion = fields("conclusion").convertTo[HippoSequent])
      case "coreAxiom" => HippoProof.CoreAxiom(name = fields("name").convertTo[String])
      case "coreAxiomaticRule" => HippoProof.CoreAxiomaticRule(name = fields("name").convertTo[String])
      case "coreProofRule" => HippoProof.CoreProofRule(
          conclusion = fields("conclusion").convertTo[HippoSequent],
          rule = fields("rule").convertTo[Rule],
        )
      case "uRename" =>
        HippoProof.URename(proof = proofs(fields("proof").convertTo[Int]), rename = fields("rename").convertTo[URename])
      case "uSubst" =>
        HippoProof.USubst(proof = proofs(fields("proof").convertTo[Int]), subst = fields("subst").convertTo[USubst])
      case "globallySoundUSubst" => HippoProof.GloballySoundUSubst(
          premise = fields("premise").convertTo[HippoSequent],
          subst = fields("subst").convertTo[USubst],
        )
      case "join" => HippoProof.Join(
          proof = proofs(fields("proof").convertTo[Int]),
          subproof = proofs(fields("subproof").convertTo[Int]),
          at = fields("at").convertTo[Int],
        )
      case "swap" => HippoProof.Swap(
          proof = proofs(fields("proof").convertTo[Int]),
          premise1 = fields("premise1").convertTo[Int],
          premise2 = fields("premise2").convertTo[Int],
        )
      case "deduplicate" => HippoProof.Deduplicate(
          proof = proofs(fields("proof").convertTo[Int]),
          premise = fields("premise").convertTo[Int],
          duplicate = fields("duplicate").convertTo[Int],
        )
      case "weaken" => HippoProof
          .Weaken(proof = proofs(fields("proof").convertTo[Int]), premise = fields("premise").convertTo[HippoSequent])

    }

    proofs.append(proof)
  }

  def proofsToJson(proofs: Seq[HippoProof]): JsValue = {
    val jsProofs = mutable.Buffer.empty[JsValue]
    val indexByProof = new util.IdentityHashMap[HippoProof, Int]()
    val proofIndices = proofs.map(addProof(jsProofs, indexByProof, _))
    JsObject("proofs" -> proofIndices.toJson, "parts" -> jsProofs.toVector.toJson)
  }

  def jsonToProofs(json: JsValue): Seq[HippoProof] = {
    val fields = json.asJsObject.fields
    val proofs = mutable.IndexedBuffer.empty[HippoProof]
    fields("parts").convertTo[Seq[JsValue]].foreach(addJsProof(proofs, _))
    fields("proofs").convertTo[Seq[Int]].map(proofs)
  }
}
