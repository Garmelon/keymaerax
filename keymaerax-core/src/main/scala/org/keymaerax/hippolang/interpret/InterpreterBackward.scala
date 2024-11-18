/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.core.Sequent
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.{HippoExpression, HippoValue, HlangException}
import org.keymaerax.hippolochos.BackwardTactic
import org.keymaerax.hippolochos.run.{HippoContext, ProofChain}

class InterpreterBackward(ictx: HippoInterpreterContext, ctx: HippoContext, conclusion: Sequent)
    extends InterpreterPure(ictx, ctx) {

  var chain: ProofChain = ctx.chain(conclusion)

  override def eval(namespace: MutableNamespace, expr: HippoExpression): HippoValue = expr match {
    case e: HippoExpression.ApplyTactic =>
      val target = eval(namespace, e.target) match {
        case HippoValue.Tactic(t: BackwardTactic) => t
        case _ => throw HlangException(
            "only backward tactics can be tactic-applied in this context",
            slice = e.target.slice,
            label = "this is not a backward tactic",
          )
      }

      val arg = e.args match {
        case Seq() => 0
        case Seq(arg) => eval(namespace, arg).asInt
        case _ => throw HlangException("too many arguments", slice = e.argsSlice, label = "only 0 or 1 args allowed")
      }

      chain = chain.backwardAt(arg)(target)

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
