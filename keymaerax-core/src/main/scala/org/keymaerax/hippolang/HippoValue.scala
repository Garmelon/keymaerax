/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang

import org.keymaerax.core.Sequent
import org.keymaerax.hippolang.namespace.ImmutableNamespace
import org.keymaerax.hippolib.primitive.Graph
import org.keymaerax.hippolochos.proof.HippoProof
import org.keymaerax.{core, hippolochos}

sealed trait HippoValue {
  def isTruthy: Boolean = true
  def asInt: Int = throw new IllegalArgumentException("value is not an integer")
  def asString: String = throw new IllegalArgumentException("value is not a string")
  def asSequent: Sequent = throw new IllegalArgumentException("value is not a dL sequent")
  def asTactic: hippolochos.Tactic = throw new IllegalArgumentException("value is not a tactic")
}

object HippoValue {
  final case object Null extends HippoValue {
    override def isTruthy: Boolean = false
  }

  final case class Bool(value: scala.Boolean) extends HippoValue {
    override def isTruthy: Boolean = value
  }

  final case class Int(value: scala.Int) extends HippoValue {
    override def asInt: scala.Int = value
  }

  final case class String(value: java.lang.String) extends HippoValue {
    override def asString: java.lang.String = value
  }

  final case class List(values: IndexedSeq[HippoValue]) extends HippoValue

  final case class DlExpression(value: core.Expression) extends HippoValue

  final case class DlSequent(value: core.Sequent) extends HippoValue {
    override def asSequent: core.Sequent = value
  }

  final case class Namespace(value: ImmutableNamespace) extends HippoValue

  final case class Proof(value: HippoProof) extends HippoValue

  final case class Tactic(value: hippolochos.Tactic) extends HippoValue {
    override def asTactic: hippolochos.Tactic = value
  }

  final case class ProofInfo(value: org.keymaerax.hippolib.meta.ProofInfo) extends HippoValue

  final case class TacticInfo(value: org.keymaerax.hippolib.meta.TacticInfo) extends HippoValue

  // Function-like

  final case class BuiltinFunction(value: org.keymaerax.hippolang.BuiltinFunction) extends HippoValue

  final case class BuiltinMemberFunction(target: HippoValue, value: org.keymaerax.hippolang.BuiltinMemberFunction)
      extends HippoValue

  final case class Function(env: ImmutableNamespace, args: Seq[HippoIdentifier], body: HippoExpression)
      extends HippoValue

  // For graph { ... }

  // TODO When migrating to Scala 3: GraphNode(graph: Graph.Builder)(node: graph.Var)
  final case class GraphNode(graph: Graph.Builder, node: Graph.Builder#Var) extends HippoValue {
    // Check if the node belongs to the graph (which it should always do).
    // This is a band-aid fix for classes not properly supporting path-dependent types in Scala 2.
    // We should be able to remove it once we migrate to Scala 3.
    node.belongingTo(graph)
  }
}
