/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus

//import org.keymaerax.hippolochus.interpret.exceptions
//
//import scala.reflect.ClassTag
//
//trait HippoFunction {
//  def executeWithArgs(args: Seq[HippoValue]): HippoValue
//}
//
//object HippoFunction {
//  case class FromScala1[A1 <: HippoValue: ClassTag, R <: HippoValue](function: A1 => R) extends HippoFunction {
//    override def executeWithArgs(args: Seq[HippoValue]): HippoValue = {
//      CriticalException.requireNumberOfArgs(args, 1)
//      val Seq(arg1) = args
//      function(CriticalException.castBelleValue[A1](arg1))
//    }
//
//    def apply(a1: A1): R = function(a1)
//  }
//
//  case class FromScala2[A1 <: HippoValue: ClassTag, A2 <: HippoValue: ClassTag, R <: HippoValue](
//      function: (A1, A2) => R
//  ) extends HippoFunction {
//    override def executeWithArgs(args: Seq[HippoValue]): HippoValue = {
//      CriticalException.requireNumberOfArgs(args, 2)
//      val Seq(arg1, arg2) = args
//      function(CriticalException.castBelleValue[A1](arg1), CriticalException.castBelleValue[A2](arg2))
//    }
//
//    def apply(a1: A1, a2: A2): R = function(a1, a2)
//  }
//
//  case class FromScala3[
//      A1 <: HippoValue: ClassTag,
//      A2 <: HippoValue: ClassTag,
//      A3 <: HippoValue: ClassTag,
//      R <: HippoValue,
//  ](function: (A1, A2, A3) => R)
//      extends HippoFunction {
//    override def executeWithArgs(args: Seq[HippoValue]): HippoValue = {
//      CriticalException.requireNumberOfArgs(args, 3)
//      val Seq(arg1, arg2, arg3) = args
//      function(
//        CriticalException.castBelleValue[A1](arg1),
//        CriticalException.castBelleValue[A2](arg2),
//        CriticalException.castBelleValue[A3](arg3),
//      )
//    }
//
//    def apply(a1: A1, a2: A2, a3: A3): R = function(a1, a2, a3)
//  }
//}
