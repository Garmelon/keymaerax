/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.tools

import org.keymaerax.core.{
  Close,
  CoHide2,
  Cut,
  CutLeft,
  CutRight,
  ExchangeLeftRule,
  ExchangeRightRule,
  Expression,
  Formula,
  PositionRule,
  Rule,
  SeqPos,
  Sequent,
  UniformRenaming,
}
import org.keymaerax.hippolang.namespace.ImmutableNamespace
import org.keymaerax.hippolang.{BuiltinMemberFunction, HippoExpression, HippoIdentifier, HippoValue}
import org.keymaerax.hippolochos.proof.{ExternalSource, HippoPremise, HippoProof}
import org.keymaerax.parser.FullPrettyPrinter

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest
import scala.collection.SortedMap

case class Hash(hexString: String) {
  require(hexString.matches("^[0-9a-z]{64}$"))
}

object Hash {
  private val printer = FullPrettyPrinter

  def start: Builder = new Builder

  class Builder {
    val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

    def build: Hash = Hash(digest.digest().map(b => f"$b%02x").mkString)

    def digest(int: Int): Builder = {
      digest.update(ByteBuffer.allocate(4).putInt(int))
      this
    }

    def digest(bool: Boolean): Builder = {
      digest.update(if (bool) 1.toByte else 0.toByte)
      this
    }

    def digest(str: String): Builder = {
      val bytes = str.getBytes(StandardCharsets.UTF_8)
      digest(bytes.length)
      digest.update(bytes)
      this
    }

    def digest(hash: Hash): Builder = digest(hash.hexString)

    def digest(path: Path): Builder = digest(path.toString)

    def digest(expr: Expression): Builder = digest(printer(expr))

    def digest(sequent: Sequent): Builder = digestSeq(sequent.ante)(_.digest(_))

    def digest(pos: SeqPos): Builder = digest(pos.getPos)

    def digest(rule: Rule): Builder = rule match {
      // TODO Make exhaustive
      case ExchangeRightRule(pos1, pos2) => digest("ExchangeRightRule").digest(pos1).digest(pos2)
      case ExchangeLeftRule(pos1, pos2) => digest("ExchangeLeftRule").digest(pos1).digest(pos2)
      case Close(assume, pos) => digest("Close").digest(assume).digest(pos)
      case Cut(c) => digest("Cut").digest(c)
      case UniformRenaming(what, repl) => digest("UniformRenaming").digest(what).digest(repl)
      case CoHide2(pos1, pos2) => digest("CoHide2").digest(pos1).digest(pos2)
      case CutRight(c, pos) => digest("CutRight").digest(c).digest(pos)
      case CutLeft(c, pos) => digest("CutLeft").digest(c).digest(pos)
    }

    def digest(identifier: HippoIdentifier): Builder = digest(identifier.value)

    def digest(source: ExternalSource): Builder = source match {
      case ExternalSource.Sorry => digest("Sorry")
      case ExternalSource.QeTool(formula) => digest("QeTool").digest(formula)
      case ExternalSource.Derived(hash) => digest("Derived").digest(hash)
    }

    def digest(premise: HippoPremise): Builder = {
      digest(premise.mustBeProved)
      digest(premise.sequent)
    }

    def digest(proof: HippoProof): Builder = proof match {
      case HippoProof.External(conclusion, premises, source) =>
        digest("External").digest(conclusion).digestSeq(premises)(_.digest(_)).digest(source)
      case HippoProof.Sequent(conclusion) => digest("Sequent").digest(conclusion)
      case HippoProof.CoreAxiom(name) => digest("CoreAxiom").digest(name)
      case HippoProof.CoreAxiomaticRule(name) => digest("CoreAxiomaticRule").digest(name)
      case HippoProof.CoreProofRule(conclusion, rule) => digest("CoreProofRule").digest(conclusion).digest(rule)
      case HippoProof.URename(proof, rename) => ???
      case HippoProof.USubst(proof, subst) => ???
      case HippoProof.GloballySoundUSubst(premise, subst) => ???
      case HippoProof.Join(proof, subproof, at) => ???
      case HippoProof.Swap(proof, premise1, premise2) => ???
      case HippoProof.Deduplicate(proof, premise, duplicate) => ???
      case HippoProof.Weaken(proof, premise) => ???
    }

    def digest(builtin: BuiltinMemberFunction): Builder = builtin match {
      case BuiltinMemberFunction.Forward => digest("Forward")
      case BuiltinMemberFunction.Backward => digest("Backward")
      case BuiltinMemberFunction.Pure => digest("Pure")
    }

    def digest(value: HippoValue): Builder = value match {
      case HippoValue.Null => digest("Null")
      case HippoValue.Bool(value) => digest("Bool").digest(value)
      case HippoValue.Int(value) => digest("Int").digest(value)
      case HippoValue.String(value) => digest("String").digest(value)
      case HippoValue.List(values) => digest("List").digestSeq(values)(_.digest(_))
      case HippoValue.DlExpression(value) => digest("DlExpression").digest(value)
      case HippoValue.DlSequent(value) => digest("DlExpression").digest(value)
      case HippoValue.Namespace(value) => digest("Namespace").digest(value.hash)
      case HippoValue.Proof(value) => digest("Proof").digest(value)
      case HippoValue.Tactic(value) => digest("Tactic").digest(value.hash)
      case HippoValue.ProofInfo(value) => digest("ProofInfo").digest(value.proof)
      case HippoValue.TacticInfo(value) => ???
      case HippoValue.BuiltinFunction(value) => ???
      case HippoValue.BuiltinMemberFunction(target, value) => ???
      case HippoValue.Function(env, args, body) => ???
      case HippoValue.GraphNode(graph, node) => ???
    }

    def digest(expr: HippoExpression): Builder = expr match {
      case HippoExpression.Const(value) => digest("Const").digest(value)
      case HippoExpression.Import(path) => digest("Import").digest(path)
      case HippoExpression.Declare(exports, mutable, name, value) =>
        digest("Declare").digest(exports).digest(mutable).digest(name).digest(value)
      case HippoExpression.Assign(name, value) => digest("Assign").digest(name).digest(value)
      case HippoExpression.Lookup(name) => digest("Lookup").digest(name)
      case HippoExpression.If(condition, ifTrue, ifFalse) =>
        digest("If").digest(condition).digest(ifTrue).digestOpt(ifFalse)(_.digest(_))
      case HippoExpression.While(condition, body) => digest("While").digest(condition).digest(body)
      case HippoExpression.Function(args, body) => digest("Function").digestSeq(args)(_.digest(_)).digest(body)
      case HippoExpression.Theorem(conclusion, premises, proof) =>
        digest("Theorem").digest(conclusion).digestSeq(premises)(_.digest(_)).digest(proof)
      case HippoExpression.Sequence(exprs, returnExpr) =>
        digest("Sequence").digestSeq(exprs)(_.digest(_)).digestOpt(returnExpr)(_.digest(_))
      case HippoExpression.Block(inner) => digest("Block").digest(inner)
      case HippoExpression.BackwardBlock(inner) => digest("BackwardBlock").digest(inner)
      case HippoExpression.GraphBlock(inner) => digest("GraphBlock").digest(inner)
      case HippoExpression.BuiltinAccess(target, member) => digest("BuiltinAccess").digest(target).digest(member)
      case HippoExpression.Access(target, name) => digest("Access").digest(target).digest(name)
      case HippoExpression.Apply(target, args) => digest("Apply").digest(target).digestSeq(args)(_.digest(_))
      case HippoExpression.ApplyTactic(target, args) =>
        digest("ApplyTactic").digest(target).digestSeq(args)(_.digest(_))
    }

    def digestOpt[T](opt: Option[T])(digestInner: (Builder, T) => Unit): Builder = {
      digest(opt.isDefined)
      opt.foreach(digestInner(this, _))
      this
    }

    def digestSeq[T](seq: Seq[T])(digestInner: (Builder, T) => Unit): Builder = {
      digest(seq.length)
      seq.foreach(digestInner(this, _))
      this
    }

    def digestMap[K, V](map: SortedMap[K, V])(digestInner: (Builder, K, V) => Unit): Builder = {
      digest(map.size)
      map.foreach { case (k, v) => digestInner(this, k, v) }
      this
    }
  }
}
