/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochos.tools

import org.keymaerax.core.{Formula, Sequent}
import org.keymaerax.hippolochos.proof.HippoPremise
import org.keymaerax.parser.FullPrettyPrinter

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

case class Hash(hexString: String) {
  require(hexString.matches("^[0-9a-z]{64}$"))
}

object Hash {
  private val printer = FullPrettyPrinter

  private def hash(body: MessageDigest => Unit): Hash = {
    val digest = MessageDigest.getInstance("SHA-256")
    body(digest)
    Hash(digest.digest().map(b => f"$b%02x").mkString)
  }

  private def digestInt(digest: MessageDigest, int: Int): Unit = digest.update(ByteBuffer.allocate(4).putInt(int))

  private def digestBool(digest: MessageDigest, bool: Boolean): Unit = digest.update(if (bool) 1.toByte else 0.toByte)

  private def digestStr(digest: MessageDigest, str: String): Unit = {
    val bytes = str.getBytes(StandardCharsets.UTF_8)
    digestInt(digest, bytes.length)
    digest.update(bytes)
  }

  private def digestSeq[T](digest: MessageDigest, seq: Seq[T], digestItem: (MessageDigest, T) => Unit): Unit = {
    digestInt(digest, seq.length)
    seq.foreach(digestItem(digest, _))
  }

  private def digestFormula(digest: MessageDigest, formula: Formula): Unit = digestStr(digest, printer(formula))

  private def digestSequent(digest: MessageDigest, sequent: Sequent): Unit =
    digestSeq[Formula](digest, sequent.ante, digestFormula)

  private def digestHippoPremise(digest: MessageDigest, premise: HippoPremise): Unit = {
    digestBool(digest, premise.mustBeProved)
    digestSequent(digest, premise.sequent)
  }

  def ofFormula(formula: Formula): Hash = hash { digestFormula(_, formula) }

  def ofProofShape(conclusion: Sequent, premises: IndexedSeq[HippoPremise]): Hash = hash { digest =>
    digestSequent(digest, conclusion)
    digestSeq(digest, premises, digestHippoPremise)
  }
}
