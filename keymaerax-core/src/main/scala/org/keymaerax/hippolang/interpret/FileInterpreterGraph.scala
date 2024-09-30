/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.{HippoExpression, HippoValue}
import org.keymaerax.hippolib.primitive.Graph
import org.keymaerax.hippolochos.run.HippoContext

import java.nio.file.Path

class FileInterpreterGraph(ictx: HippoInterpreterContext, ctx: HippoContext, file: Option[Path])
    extends FileInterpreter(ictx, ctx, file) {

  var graph: Graph.Builder = Graph.newBuilder

  override protected def applyBuiltinFunctionPremise(args: IndexedSeq[HippoValue]): HippoValue = args match {
    case Seq(HippoValue.Int(i)) if i >= 0 => HippoValue.GraphNode(graph, graph.premise(i))
    case Seq(HippoValue.Int(_)) => throw new IllegalArgumentException("premise id must not be negative")
    case Seq(_) => throw new IllegalArgumentException("premise id must be a nonnegative integer")
    case _ => throw new IllegalArgumentException("exactly one argument required")
  }

  override protected def evalApplyTactic(namespace: MutableNamespace, expr: HippoExpression.ApplyTactic): HippoValue = {
    val targetV = eval(namespace, expr.target) match {
      case HippoValue.Tactic(t) => t
      case _ => throw new Exception("target is not a tactic")
    }

    val argsV = expr
      .args
      .map(eval(namespace, _))
      .map {
        case HippoValue.GraphNode(graph, node) if graph eq this.graph => node.belongingTo(this.graph)
        case HippoValue.GraphNode(_, _) => throw new IllegalArgumentException("graph node must belong to current graph")
        case _ => throw new IllegalArgumentException("only graph nodes are allowed in square brackets in graph blocks")
      }

    val node = this.graph.stepAny(targetV, argsV)
    HippoValue.GraphNode(graph, node)
  }
}

object FileInterpreterGraph {
  def tactic(
      ictx: HippoInterpreterContext,
      ctx: HippoContext,
      file: Option[Path],
      namespace: ImmutableNamespace,
      expr: HippoExpression,
  ): Graph = {
    val innerInterp = new FileInterpreterGraph(ictx, ctx, file)
    val innerNs = new MutableNamespace(child = Some(namespace))
    val conclusion = innerInterp.eval(innerNs, expr)
    conclusion match {
      case HippoValue.GraphNode(graph, node) =>
        if (graph eq innerInterp.graph) graph.build(node.belongingTo(graph))
        else throw new Exception("mixing nodes of different graphs is not allowed")
      case _ => throw new Exception("graph block must return graph node")
    }
  }
}
