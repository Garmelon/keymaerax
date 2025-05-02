/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolang.interpret

import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippolang.namespace.{ImmutableNamespace, MutableNamespace}
import org.keymaerax.hippolang.{BuiltinFunction, HlangException, HlangExpression, HlangValue}
import org.keymaerax.hippolib.primitive.Graph

import scala.collection.mutable

class InterpreterGraph(ictx: InterpreterContext, ctx: HippoContext) extends InterpreterPure(ictx, ctx) {
  override def during: String = "during graph evaluation"

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

  override def eval(namespace: MutableNamespace, expr: HlangExpression): HlangValue = expr match {
    case e: HlangExpression.ApplyTactic =>
      val target = eval(namespace, e.target) match {
        case HlangValue.Tactic(t) => t
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
          case (_, HlangValue.Int(i)) => getNode(i)
          case (slice, _) => throw HlangException("argument must be an integer (i.e. a node id)", slice = slice)
        }

      val node = this.graph.stepAny(target, Args)
      HlangValue.Int(registerNode(node))

    case _ => super.eval(namespace, expr)
  }

  override def applyValue(e: HlangExpression.Apply, target: HlangValue, args: IndexedSeq[HlangValue]): HlangValue =
    target match {
      case HlangValue.BuiltinFunction(BuiltinFunction.Premise) => args match {
          case Seq(HlangValue.Int(i)) if i >= 0 =>
            HlangValue.Int(premises.getOrElseUpdate(i, registerNode(graph.premise(i))))
          case Seq(HlangValue.Int(_)) => throw new IllegalArgumentException("premise id must not be negative")
          case Seq(_) => throw new IllegalArgumentException("premise id must be a nonnegative integer")
          case _ => throw new IllegalArgumentException("exactly one argument required")
        }

      case _ => super.applyValue(e, target, args)
    }
}

object InterpreterGraph {
  def tactic(
      ictx: InterpreterContext,
      ctx: HippoContext,
      namespace: ImmutableNamespace,
      expr: HlangExpression,
  ): Graph = {
    val innerInterp = new InterpreterGraph(ictx, ctx)
    val innerNs = new MutableNamespace(child = Some(namespace))
    val conclusion = innerInterp.eval(innerNs, expr)
    conclusion match {
      case HlangValue.Int(node) => innerInterp.graph.build(innerInterp.getNode(node))
      case _ => throw new Exception("graph block must return graph node")
    }
  }
}
