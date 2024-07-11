/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.interpret

/**
 * A non-critical exception only halts program execution if it isn't caught by the program.
 *
 * Examples for non-critical situations:
 *
 *   - A tactic fails to make progress on its goal.
 *   - A tactic is applied to a type of formula it doesn't support.
 *
 * @see
 *   [[CriticalException]]
 */
class NonCriticalException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)
