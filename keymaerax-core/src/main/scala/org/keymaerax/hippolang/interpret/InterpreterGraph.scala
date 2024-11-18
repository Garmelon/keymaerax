/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.{BuiltinFunction, HippoExpression, HippoValue, HlangException}
import org.keymaerax.hippolib.primitive.Graph
import org.keymaerax.hippolochos.run.HippoContext

import scala.collection.mutable

class InterpreterGraph(ictx: HippoInterpreterContext, ctx: HippoContext) extends InterpreterPure(ictx, ctx) {
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

  override def eval(namespace: MutableNamespace, expr: HippoExpression): HippoValue = expr match {
    case e: HippoExpression.ApplyTactic =>
      val target = eval(namespace, e.target) match {
        case HippoValue.Tactic(t) => t
        case _ => throw HlangException(
            "only tactics can be tactic-applied in this context",
            slice = e.target.slice,
            label = "this is not a tactic",
          )
      }

      val Args = e
        .args
        .map(arg => (arg.slice, eval(namespace, arg)))
        .map {
          case (_, HippoValue.Int(i)) => getNode(i)
          case (slice, _) => throw HlangException("argument must be an integer (i.e. a node id)", slice = slice)
        }

      val node = this.graph.stepAny(target, Args)
      HippoValue.Int(registerNode(node))

    case _ => super.eval(namespace, expr)
  }

  override def applyValue(target: HippoValue, args: IndexedSeq[HippoValue]): HippoValue = target match {
    case HippoValue.BuiltinFunction(BuiltinFunction.Premise) => args match {
        case Seq(HippoValue.Int(i)) if i >= 0 =>
          HippoValue.Int(premises.getOrElseUpdate(i, registerNode(graph.premise(i))))
        case Seq(HippoValue.Int(_)) => throw new IllegalArgumentException("premise id must not be negative")
        case Seq(_) => throw new IllegalArgumentException("premise id must be a nonnegative integer")
        case _ => throw new IllegalArgumentException("exactly one argument required")
      }

    case _ => super.applyValue(target, args)
  }
}

object InterpreterGraph {
  def tactic(
      ictx: HippoInterpreterContext,
      ctx: HippoContext,
      namespace: ImmutableNamespace,
      expr: HippoExpression,
  ): Graph = {
    val innerInterp = new InterpreterGraph(ictx, ctx)
    val innerNs = new MutableNamespace(child = Some(namespace))
    val conclusion = innerInterp.eval(innerNs, expr)
    conclusion match {
      case HippoValue.Int(node) => innerInterp.graph.build(innerInterp.getNode(node))
      case _ => throw new Exception("graph block must return graph node")
    }
  }
}
