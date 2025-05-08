/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore.tools

import org.keymaerax.core.{
  AndLeft,
  AndRight,
  AnyArg,
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
  Except,
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
  Provable,
  Rule,
  SeqPos,
  Sequent,
  Skolemize,
  Space,
  SubstitutionPair,
  URename,
  USubst,
  UniformRenaming,
}
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

trait Hashable {
  def digestInto(hasher: Hasher): Unit
}

object Hasher {
  private val printer = FullPrettyPrinter

  def apply(): Hasher = new Hasher()
}

class Hasher {
  val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

  def hash: Hash = Hash(digest.digest().map(b => f"$b%02x").mkString)

  def digest[T <: Hashable](hashable: T): Hasher = {
    hashable.digestInto(this)
    this
  }

  /////////////////////
  // Primitive types //
  /////////////////////

  def digest(int: Int): Hasher = {
    digest.update(ByteBuffer.allocate(4).putInt(int).rewind())
    this
  }

  def digest(bool: Boolean): Hasher = {
    digest.update(if (bool) 1.toByte else 0.toByte)
    this
  }

  def digest(str: String): Hasher = {
    val bytes = str.getBytes(StandardCharsets.UTF_8)
    digest(bytes.length)
    digest.update(bytes)
    this
  }

  def digest(hash: Hash): Hasher = digest(hash.hexString)

  def digest(path: Path): Hasher = digest(path.toString)

  def digest(clazz: Class[_]): Hasher = {
    val loaderName = Option(clazz.getClassLoader).flatMap(it => Option(it.getName))
    digestOptWith(loaderName)(_.digest(_)).digest(clazz.getName)
  }

  def digest[C](implicit ct: ClassTag[C]): Hasher = digest(ct.runtimeClass)

  ////////////////
  // Containers //
  ////////////////

  def digestOptWith[T](opt: Option[T])(digestInner: (Hasher, T) => Unit): Hasher = {
    digest(opt.isDefined)
    opt.foreach(digestInner(this, _))
    this
  }

  def digestOpt[T <: Hashable](opt: Option[T]): Hasher = digestOptWith(opt)(_.digest(_))

  def digestSeqWith[T](seq: Seq[T])(digestInner: (Hasher, T) => Unit): Hasher = {
    digest(seq.length)
    seq.foreach(digestInner(this, _))
    this
  }

  def digestSeq[T <: Hashable](seq: Seq[T]): Hasher = digestSeqWith(seq)(_.digest(_))

  def digestMapWith[K, V](map: SortedMap[K, V])(digestInner: (Hasher, K, V) => Unit): Hasher = {
    digest(map.size)
    map.foreach { case (k, v) => digestInner(this, k, v) }
    this
  }

  def digestMap[K <: Hashable, V <: Hashable](map: SortedMap[K, V]): Hasher = digestMapWith(map)(_.digest(_).digest(_))

  //////////////////
  // Kernel types //
  //////////////////

  def digest(space: Space): Hasher = space match {
    case AnyArg => digest("AnyArg")
    case Except(taboos) => digest("Except").digestSeqWith(taboos)(_.digest(_))
  }

  def digest(expr: Expression): Hasher = digest(Hasher.printer(expr))

  def digest(sequent: Sequent): Hasher = digestSeqWith(sequent.ante)(_.digest(_))
    .digestSeqWith(sequent.succ)(_.digest(_))

  def digest(provable: Provable): Hasher = digest(provable.conclusion).digestSeqWith(provable.subgoals)(_.digest(_))

  def digest(pos: SeqPos): Hasher = digest(pos.getPos)

  def digest(rule: Rule): Hasher = rule match {
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

  def digest(rename: URename): Hasher = digest(rename.what).digest(rename.repl).digest(rename.semantic)

  def digest(subst: USubst): Hasher = digestSeqWith(subst.subsDefsInput) { (b, p) => b.digest(p.what).digest(p.repl) }

  def digest(substPair: SubstitutionPair): Hasher = digest(substPair.what).digest(substPair.repl)
}
