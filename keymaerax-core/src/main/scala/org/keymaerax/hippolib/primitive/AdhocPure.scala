/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.primitive

import org.keymaerax.hippolochos.PureTactic
import org.keymaerax.hippolochos.tools.Hash

abstract class AdhocPure extends PureTactic {
  require(getClass.isAnonymousClass, "AdhocPure must only be extended by anonymous classes")

  // Assuming this class is only ever overridden anonymously, this *should* be fine.
  // The class loader name is included because classes from different class loaders may share the same name.
  override lazy val hash: Hash = Hash
    .start
    .digestOpt(Option(getClass.getClassLoader).map(_.getName))(_.digest(_))
    .digest(getClass.getName)
    .build
}
