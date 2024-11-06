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

class InterpreterBackward(ictx: HippoInterpreterContext, ctx: HippoContext, conclusion: Sequent)
    extends InterpreterPure(ictx, ctx) {

  var chain: ProofChain = ctx.chain(conclusion)

  override def eval(namespace: MutableNamespace, expr: HippoExpression): HippoValue = expr match {
    case HippoExpression.ApplyTactic(target, args) =>
      val targetV = eval(namespace, target) match {
        case HippoValue.Tactic(t: BackwardTactic) => t
        case _ => throw new Exception("target is not a backward tactic")
      }

      val argV = args match {
        case Seq() => 0
        case Seq(arg) => eval(namespace, arg).asInt
        case _ => throw new Exception("too many args for tactic application")
      }

      chain = chain.backwardAt(argV)(targetV)

      HippoValue.Null

    case _ => super.eval(namespace, expr)
  }
}

object InterpreterBackward {
  def tactic(
      ictx: HippoInterpreterContext,
      namespace: ImmutableNamespace,
      expr: HippoExpression,
  ): InterpreterBackwardTactic = InterpreterBackwardTactic(ictx, namespace, expr)
}
