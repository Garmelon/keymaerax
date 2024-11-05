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
import scala.collection.mutable

class FileInterpreterGraph(ictx: HippoInterpreterContext, ctx: HippoContext, file: Option[Path])
    extends FileInterpreter(ictx, ctx, file) {

  val graph: Graph.Builder = Graph.newBuilder
  val nodes: mutable.IndexedBuffer[graph.Var] = mutable.IndexedBuffer.empty
  val premises: mutable.Map[Int, Int] = mutable.Map.empty // Maps from premise index to node id

  private def registerNode(node: graph.Var): Int = {
    nodes.append(node)
    nodes.length - 1
  }

  private def getNode(id: Int): graph.Var = {
    require(0 <= id && id < nodes.length, "id is out of bounds")
    nodes(id)
  }

  override protected def applyBuiltinFunctionPremise(args: IndexedSeq[HippoValue]): HippoValue = args match {
    case Seq(HippoValue.Int(i)) if i >= 0 => HippoValue.Int(premises.getOrElseUpdate(i, registerNode(graph.premise(i))))
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
        case HippoValue.Int(i) => getNode(i)
        case _ => throw new IllegalArgumentException("only node ids are allowed in square brackets in graph blocks")
      }

    val node = this.graph.stepAny(targetV, argsV)
    HippoValue.Int(registerNode(node))
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
      case HippoValue.Int(node) => innerInterp.graph.build(innerInterp.getNode(node))
      case _ => throw new Exception("graph block must return graph node")
    }
  }
}
