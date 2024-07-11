/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolochus.proof

import org.keymaerax.core
import org.keymaerax.core.Sequent
import org.keymaerax.hippolochus.axiom.HippoAxiom
import org.keymaerax.hippolochus.rule.HippoRule

sealed trait HippoEvidence {
  def computeProvable: core.Provable
}

object HippoEvidence {
  case class Sorry() extends HippoEvidence {
    override def computeProvable: core.Provable = ???
  }

  case class Trivial(sequent: Sequent) extends HippoEvidence {
    override def computeProvable: core.Provable = core.Provable.startProof(sequent)
  }

  case class Provable(provable: core.Provable) extends HippoEvidence {
    override def computeProvable: core.Provable = provable
  }

  case class Axiom(axiom: HippoAxiom) extends HippoEvidence {
    override def computeProvable: core.Provable = axiom.proof.computeProvable
  }

  case class Rule(rule: HippoRule) extends HippoEvidence {
    override def computeProvable: core.Provable = rule.proof.computeProvable
  }

  case class Extended(root: HippoProof, index: Int, extension: HippoProof) extends HippoEvidence {
    // Early sanity checks to prevent frustration down the line.
    require(root.resolveIndex(index).isDefined, "invalid index")
    require(root.goal(index).sequent == extension.conclusion, "incompatible extension")

    override def computeProvable: core.Provable = root.computeProvable.apply(extension.computeProvable, index)
  }

  case class ExtendedMany(root: HippoProof, extensions: Map[Int, HippoProof]) extends HippoEvidence {
    // Early sanity checks to prevent frustration down the line.
    for ((index, extension) <- extensions) {
      require(root.resolveIndex(index).isDefined, "invalid index")
      require(root.goal(index).sequent == extension.conclusion, "incompatible extension")
    }

    override def computeProvable: core.Provable = extensions
      .toSeq
      .sortBy(_._1)
      .foldRight(root.computeProvable) { case ((index, extension), provable) =>
        provable.apply(extension.computeProvable, index)
      }
  }
}
