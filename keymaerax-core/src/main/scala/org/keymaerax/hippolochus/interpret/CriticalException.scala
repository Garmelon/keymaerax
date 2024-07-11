/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.interpret

import org.keymaerax.hippolochus.proof.HippoProof
import org.keymaerax.hippolochus.{HippoIdentifier, HippoValue}

import scala.collection.immutable.LinearSeq
import scala.reflect.{classTag, ClassTag}

/**
 * A critical exception immediately halts program evaluation. It is thrown when it doesn't make sense for the program to
 * continue running.
 *
 * Examples for critical situations:
 *
 *   - A function is supplied the incorrect number or type of arguments.
 *   - A theorem is not fully proven by its expression.
 *   - A tactic is executed outside of a proof context.
 *
 * @see
 *   [[NonCriticalException]]
 */
class CriticalException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

object CriticalException {
  def cast[T <: HippoValue: ClassTag](value: HippoValue): T =
    if (classTag.runtimeClass.isInstance(value)) { value.asInstanceOf[T] }
    else { throw new WrongTypeException(value, classTag.runtimeClass) }

  def args0(args: Seq[HippoValue]): Unit = if (args.nonEmpty) throw new WrongArgCount()

  def args1[T1 <: HippoValue: ClassTag](args: Seq[HippoValue]): T1 = args match {
    case Seq(v1) => cast[T1](v1)
    case _ => throw new WrongArgCount()
  }

  def args2[T1 <: HippoValue: ClassTag, T2 <: HippoValue: ClassTag](args: Seq[HippoValue]): (T1, T2) = args match {
    case Seq(v1, v2) => (cast[T1](v1), cast[T2](v2))
    case _ => throw new WrongArgCount()
  }
}

/** Caused by a bug that's the KeYmaera X devs' fault. */
class InternalException(message: String, cause: Throwable = null) extends CriticalException(message, cause)

class NotInProofException() extends CriticalException(message = "proof context required")

class TooFewGoals() extends CriticalException(message = "too few goals in current proof context")

class TooManyGoals() extends CriticalException(message = "too many goals in current proof context")

class NameNotFoundException(namespace: Namespace, name: HippoIdentifier)
    extends CriticalException(message = s"$name not found in current namespace")

class NameAlreadyDefinedException(namespace: Namespace, name: HippoIdentifier)
    extends CriticalException(message = s"$name already defined in current namespace")

class WrongArgCount() extends CriticalException(message = "wrong number of arguments")

class WrongTypeException(actual: HippoValue, expected: Class[_]*)
    extends CriticalException(message =
      s"wrong type, expected ${expected.map(_.getSimpleName).mkString(" or ")} but got ${actual.getClass.getSimpleName}"
    )

class InvalidLabelException(provable: HippoProof, label: HippoValue)
    extends CriticalException(message =
      s"invalid label $label, expected ${provable.subgoals.flatMap(_.label).mkString(" or ")}"
    )
