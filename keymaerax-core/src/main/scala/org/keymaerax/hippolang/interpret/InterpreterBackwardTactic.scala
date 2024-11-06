/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.core.Sequent
import org.keymaerax.hippolang.HippoExpression
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolochos.BackwardTactic
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.hippolochos.run.HippoContext
import org.keymaerax.hippolochos.tools.Hash

case class InterpreterBackwardTactic(
    ictx: HippoInterpreterContext,
    namespace: ImmutableNamespace,
    expr: HippoExpression,
) extends BackwardTactic {
  override lazy val hash: Hash = Hash
    .start
    .digest[this.type]
    .digest(ictx.hash)
    .digest(namespace.hash)
    .digest(expr)
    .build

  override def runBackward(ctx: HippoContext, conclusion: Sequent, premises: Map[Int, Sequent]): HippoProof = {
    val innerInterp = new InterpreterBackward(ictx, ctx, conclusion)
    val innerNs = new MutableNamespace(child = Some(namespace))
    val _ = innerInterp.eval(innerNs, expr)
    innerInterp.chain.proof
  }
}
