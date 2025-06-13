/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippocore.ForwardTactic
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippolang.HlangConversions.HippoProofConversion
import org.keymaerax.hippolang.HlangExpression
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}

case class InterpreterForwardTactic(
    ictx: InterpreterContext,
    namespace: ImmutableNamespace,
    expr: HlangExpression.ForwardBlock,
) extends ForwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(ictx.hash).digest(namespace.hash).digest(expr).hash

  override def runForward(ctx: HippoContext, premises: IndexedSeq[HippoSequent]): HippoProof = {
    require(expr.premises.length == premises.length)
    val innerInterp = new InterpreterForward(ictx, ctx, expr)

    // Like InterpreterPure#applyFunction
    val innerNs = new MutableNamespace(child = Some(namespace))
    for ((name, premise) <- expr.premises.zip(premises)) innerNs
      .declare(name, ctx.sequent(premise).toHValue, mutable = false)
    val result = innerInterp.eval(innerNs, expr.inner)

    result.asProof
  }
}
