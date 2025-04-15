/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.core.Sequent
import org.keymaerax.hippocore.BackwardTactic
import org.keymaerax.hippocore.proof.HippoProof
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.{Hash, Hasher}
import org.keymaerax.hippolang.HippoExpression
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}

case class InterpreterBackwardTactic(
    ictx: HippoInterpreterContext,
    namespace: ImmutableNamespace,
    expr: HippoExpression.BackwardBlock,
) extends BackwardTactic {
  override lazy val hash: Hash = Hasher().digest[this.type].digest(ictx.hash).digest(namespace.hash).digest(expr).hash

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = {
    val innerInterp = new InterpreterBackward(ictx, ctx, expr, conclusion)
    val innerNs = new MutableNamespace(child = Some(namespace))
    val _ = innerInterp.eval(innerNs, expr.inner)
    innerInterp.outputProof()
  }
}
