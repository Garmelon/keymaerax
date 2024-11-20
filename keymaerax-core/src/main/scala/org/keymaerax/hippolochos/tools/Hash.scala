/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.tools

import org.keymaerax.core.{
  AndLeft,
  AndRight,
  BoundRenaming,
  Close,
  CloseFalse,
  CloseTrue,
  CoHide2,
  CoHideLeft,
  CoHideRight,
  CommuteEquivLeft,
  CommuteEquivRight,
  Cut,
  CutLeft,
  CutRight,
  EquivLeft,
  EquivRight,
  EquivifyRight,
  ExchangeLeftRule,
  ExchangeRightRule,
  Expression,
  HideLeft,
  HideRight,
  ImplyLeft,
  ImplyRight,
  NotLeft,
  NotRight,
  OrLeft,
  OrRight,
  Rule,
  SeqPos,
  Sequent,
  Skolemize,
  URename,
  USubst,
  UniformRenaming,
}
import org.keymaerax.hippolang.{BuiltinMemberFunction, HippoExpression, HippoIdentifier, HippoValue}
import org.keymaerax.hippolochos.proof.{ExternalSource, HippoPremise, HippoProof}
import org.keymaerax.parser.FullPrettyPrinter

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest
import scala.collection.SortedMap
import scala.reflect.ClassTag

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

    def digest(name: GloballyUniqueName): Builder = digest(name.name)

    def digest(hash: Hash): Builder = digest(hash.hexString)

    def digest(path: Path): Builder = digest(path.toString)

    def digest(clazz: Class[_]): Builder = {
      val loaderName = Option(clazz.getClassLoader).flatMap(it => Option(it.getName))
      digestOpt(loaderName)(_.digest(_)).digest(clazz.getName)
    }

    def digest[C](implicit ct: ClassTag[C]): Builder = digest(ct.runtimeClass)

    def digest(expr: Expression): Builder = digest(printer(expr))

    def digest(sequent: Sequent): Builder = digestSeq(sequent.ante)(_.digest(_))

    def digest(pos: SeqPos): Builder = digest(pos.getPos)

    def digest(rule: Rule): Builder = rule match {
      case HideRight(pos) => digest("HideRight").digest(pos)
      case HideLeft(pos) => digest("HideLeft").digest(pos)
      case ExchangeRightRule(pos1, pos2) => digest("ExchangeRightRule").digest(pos1).digest(pos2)
      case ExchangeLeftRule(pos1, pos2) => digest("ExchangeLeftRule").digest(pos1).digest(pos2)
      case Close(assume, pos) => digest("Close").digest(assume).digest(pos)
      case CloseTrue(pos) => digest("CloseTrue").digest(pos)
      case CloseFalse(pos) => digest("CloseFalse").digest(pos)
      case Cut(c) => digest("Cut").digest(c)
      case NotRight(pos) => digest("NotRight").digest(pos)
      case NotLeft(pos) => digest("NotLeft").digest(pos)
      case AndRight(pos) => digest("AndRight").digest(pos)
      case AndLeft(pos) => digest("AndLeft").digest(pos)
      case OrRight(pos) => digest("OrRight").digest(pos)
      case OrLeft(pos) => digest("OrLeft").digest(pos)
      case ImplyRight(pos) => digest("ImplyRight").digest(pos)
      case ImplyLeft(pos) => digest("ImplyLeft").digest(pos)
      case EquivRight(pos) => digest("EquivRight").digest(pos)
      case EquivLeft(pos) => digest("EquivLeft").digest(pos)
      case UniformRenaming(what, repl) => digest("UniformRenaming").digest(what).digest(repl)
      case BoundRenaming(what, repl, pos) => digest("BoundRenaming").digest(what).digest(repl).digest(pos)
      case Skolemize(pos) => digest("Skolemize").digest(pos)
      case CoHideRight(pos) => digest("CoHideRight").digest(pos)
      case CoHideLeft(pos) => digest("CoHideLeft").digest(pos)
      case CoHide2(pos1, pos2) => digest("CoHide2").digest(pos1).digest(pos2)
      case CutRight(c, pos) => digest("CutRight").digest(c).digest(pos)
      case CutLeft(c, pos) => digest("CutLeft").digest(c).digest(pos)
      case CommuteEquivRight(pos) => digest("CommuteEquivRight").digest(pos)
      case CommuteEquivLeft(pos) => digest("CommuteEquivLeft").digest(pos)
      case EquivifyRight(pos) => digest("EquivifyRight").digest(pos)
    }

    def digest(rename: URename): Builder = digest(rename.what).digest(rename.repl).digest(rename.semantic)

    def digest(subst: USubst): Builder = digestSeq(subst.subsDefsInput) { (b, p) => b.digest(p.what).digest(p.repl) }

    def digest(identifier: HippoIdentifier): Builder = digest(identifier.value)

    def digest(path: ExprPath): Builder = digestSeq(path.segments)(_.digest(_))

    def digest(source: ExternalSource): Builder = source match {
      case ExternalSource.Sorry => digest("Sorry")
      case ExternalSource.QeTool(formula) => digest("QeTool").digest(formula)
      case ExternalSource.Cache(hash) => digest("Cache").digest(hash)
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
      case HippoProof.URename(proof, rename) => digest("URename").digest(proof).digest(rename)
      case HippoProof.USubst(proof, subst) => digest("USubst").digest(proof).digest(subst)
      case HippoProof.GloballySoundUSubst(premise, subst) => digest("GloballySoundUSubst").digest(premise).digest(subst)
      case HippoProof.Join(proof, subproof, at) => digest("Join").digest(proof).digest(subproof).digest(at)
      case HippoProof.Swap(proof, premise1, premise2) => digest("Swap").digest(proof).digest(premise1).digest(premise2)
      case HippoProof.Deduplicate(proof, premise, duplicate) =>
        digest("Deduplicate").digest(proof).digest(premise).digest(duplicate)
      case HippoProof.Weaken(proof, premise) => digest("Weaken").digest(proof).digest(premise)
    }

    // TODO Reorganize this module so hippolang types don't "leak" into hippolochos

    def digest(builtin: BuiltinMemberFunction): Builder = builtin match {
      case BuiltinMemberFunction.Forward => digest("Forward")
      case BuiltinMemberFunction.Backward => digest("Backward")
      case BuiltinMemberFunction.Pure => digest("Pure")
      case BuiltinMemberFunction.Join => digest("Join")
      case BuiltinMemberFunction.Select => digest("Select")
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
      case HippoValue.TacticInfo(value) => digest(value.constructor.hash)
      case HippoValue.BuiltinFunction(value) => digest("BuiltinFunction").digest(value.name)
      case HippoValue.BuiltinMemberFunction(target, value) =>
        digest("BuiltinMemberFunction").digest(target).digest(value.name)
      case HippoValue.Function(env, args, body) =>
        digest("Function").digest(env.hash).digestSeq(args)(_.digest(_)).digest(body)
    }

    def digest(expr: HippoExpression): Builder = expr match {
      case HippoExpression.Const(slice, value) => digest("Const").digest(value)
      case HippoExpression.Import(slice, path) => digest("Import").digest(path)
      case HippoExpression.Declare(slice, exportSlice, mutable, name, value) =>
        digest("Declare").digest(exportSlice.isDefined).digest(mutable).digest(name).digest(value)
      case HippoExpression.Assign(slice, name, value) => digest("Assign").digest(name).digest(value)
      case HippoExpression.Lookup(slice, name) => digest("Lookup").digest(name)
      case HippoExpression.If(slice, condition, ifTrue, ifFalse) =>
        digest("If").digest(condition).digest(ifTrue).digestOpt(ifFalse)(_.digest(_))
      case HippoExpression.While(slice, condition, body) => digest("While").digest(condition).digest(body)
      case HippoExpression.Function(slice, args, body) => digest("Function").digestSeq(args)(_.digest(_)).digest(body)
      case HippoExpression.Theorem(slice, verifySlice, conclusion, premises, proof, proofSlice) => digest("Theorem")
          .digest(verifySlice.isDefined)
          .digest(conclusion)
          .digestSeq(premises)(_.digest(_))
          .digest(proof)
      case HippoExpression.Sequence(slice, exprs, returnExpr) =>
        digest("Sequence").digestSeq(exprs)(_.digest(_)).digestOpt(returnExpr)(_.digest(_))
      case HippoExpression.Block(slice, inner) => digest("Block").digest(inner)
      case HippoExpression.BackwardBlock(slice, inner) => digest("BackwardBlock").digest(inner)
      case HippoExpression.GraphBlock(slice, inner) => digest("GraphBlock").digest(inner)
      case HippoExpression.BuiltinAccess(slice, target, member) => digest("BuiltinAccess").digest(target).digest(member)
      case HippoExpression.Access(slice, target, name) => digest("Access").digest(target).digest(name)
      case HippoExpression.Apply(slice, target, args, argsSlice) =>
        digest("Apply").digest(target).digestSeq(args)(_.digest(_))
      case HippoExpression.ApplyTactic(slice, target, args, argsSlice) =>
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
