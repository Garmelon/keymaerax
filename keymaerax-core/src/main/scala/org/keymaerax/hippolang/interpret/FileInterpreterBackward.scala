/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.core.Sequent
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.{HippoExpression, HippoValue}
import org.keymaerax.hippolochos.BackwardTactic
import org.keymaerax.hippolochos.run.{HippoContext, ProofChain}

import java.nio.file.Path

class FileInterpreterBackward(ictx: HippoInterpreterContext, ctx: HippoContext, file: Option[Path], conclusion: Sequent)
    extends FileInterpreter(ictx, ctx, file) {

  var chain: ProofChain = ctx.chain(conclusion)

  override protected def evalApplyTactic(namespace: MutableNamespace, expr: HippoExpression.ApplyTactic): HippoValue = {
    val targetV = eval(namespace, expr.target) match {
      case HippoValue.Tactic(t: BackwardTactic) => t
      case _ => throw new Exception("target is not a backward tactic")
    }

    val argV = expr.args match {
      case Seq() => 0
      case Seq(arg) => eval(namespace, arg).asInt
      case _ => throw new Exception("too many args for tactic application")
    }

    chain = chain.backwardAt(argV)(targetV)

    HippoValue.Null
  }
}

object FileInterpreterBackward {
  def tactic(
      ictx: HippoInterpreterContext,
      file: Option[Path],
      namespace: ImmutableNamespace,
      expr: HippoExpression,
  ): BackwardTactic = FileInterpreterBackwardTactic(ictx, file, namespace, expr)
}
