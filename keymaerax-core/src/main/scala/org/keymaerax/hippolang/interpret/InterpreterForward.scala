/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippocore.ForwardTactic
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippolang.HlangConversions.HippoProofConversion
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.{HlangExpression, HlangValue}

class InterpreterForward(ictx: InterpreterContext, ctx: HippoContext, expr: HlangExpression.ForwardBlock)
    extends InterpreterPure(ictx, ctx) {

  override def during: String = "during forwards evaluation"

  // TODO Better error handling - do this check in parser?
  require(expr.premises.length == expr.premises.toSet.size)

  override def eval(namespace: MutableNamespace, expr: HlangExpression): HlangValue = expr match {
    case e: HlangExpression.ApplyTactic =>
      val tactic = eval(namespace, e.target).asTactic.asInstanceOf[ForwardTactic]
      val args = e.args.map(eval(namespace, _).asProof)
      ctx.forwardJoin(tactic, args*).toHValue

    case _ => super.eval(namespace, expr)
  }
}

object InterpreterForward {
  def tactic(
      ictx: InterpreterContext,
      namespace: ImmutableNamespace,
      expr: HlangExpression.ForwardBlock,
  ): InterpreterForwardTactic = InterpreterForwardTactic(ictx, namespace, expr)
}
