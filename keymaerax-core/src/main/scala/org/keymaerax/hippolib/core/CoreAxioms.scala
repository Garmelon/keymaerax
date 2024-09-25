/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core.hippolib.publish
import org.keymaerax.hippolib.meta.ProofInfo

/** Axioms from [[org.keymaerax.core.Provable.axioms]]. */
// TODO Check if the list is complete
// TODO Doc comments for every axiom
object CoreAxioms {
  @publish(name = "core.diamond")
  val diamond: ProofInfo = ProofInfo.coreAxiom("<> diamond")

  @publish(name = "core.assignbAxiom")
  val assignbAxiom: ProofInfo = ProofInfo.coreAxiom("[:=] assign")

  @publish(name = "core.assignbeq")
  val assignbeq: ProofInfo = ProofInfo.coreAxiom("[:=] assign equality")

  @publish(name = "core.selfassignb")
  val selfassignb: ProofInfo = ProofInfo.coreAxiom("[:=] self assign")

  @publish(name = "core.Dassignb")
  val Dassignb: ProofInfo = ProofInfo.coreAxiom("[':=] differential assign")

  @publish(name = "core.Dassignbeq")
  val Dassignbeq: ProofInfo = ProofInfo.coreAxiom("[':=] assign equality")

  @publish(name = "core.Dselfassignb")
  val Dselfassignb: ProofInfo = ProofInfo.coreAxiom("[':=] self assign")

  @publish(name = "core.randomb")
  val randomb: ProofInfo = ProofInfo.coreAxiom("[:*] assign nondet")

  @publish(name = "core.testb")
  val testb: ProofInfo = ProofInfo.coreAxiom("[?] test")

  @publish(name = "core.choiceb")
  val choiceb: ProofInfo = ProofInfo.coreAxiom("[++] choice")

  @publish(name = "core.composeb")
  val composeb: ProofInfo = ProofInfo.coreAxiom("[;] compose")

  @publish(name = "core.iterateb")
  val iterateb: ProofInfo = ProofInfo.coreAxiom("[*] iterate")

  @publish(name = "core.barcan")
  val barcan: ProofInfo = ProofInfo.coreAxiom("B Barcan")

  @publish(name = "core.DWbase")
  val DWbase: ProofInfo = ProofInfo.coreAxiom("DW base")

  @publish(name = "core.DE")
  val DE: ProofInfo = ProofInfo.coreAxiom("DE differential effect")

  @publish(name = "core.DEs")
  val DEs: ProofInfo = ProofInfo.coreAxiom("DE differential effect (system)")

  @publish(name = "core.DIequiv")
  val DIequiv: ProofInfo = ProofInfo.coreAxiom("DI differential invariance")

  @publish(name = "core.DGa")
  val DGa: ProofInfo = ProofInfo.coreAxiom("DG differential ghost")

  @publish(name = "core.DGpp")
  val DGpp: ProofInfo = ProofInfo.coreAxiom("DG inverse differential ghost")

  @publish(name = "core.DGi")
  val DGi: ProofInfo = ProofInfo.coreAxiom("DG inverse differential ghost implicational")

  @publish(name = "core.DGC")
  val DGC: ProofInfo = ProofInfo.coreAxiom("DG differential ghost constant")

  @publish(name = "core.DGCa")
  val DGCa: ProofInfo = ProofInfo.coreAxiom("DG differential ghost constant all")

  @publish(name = "core.DS")
  val DS: ProofInfo = ProofInfo.coreAxiom("DS& differential equation solution")

  @publish(name = "core.commaSort")
  val commaSort: ProofInfo = ProofInfo.coreAxiom(", sort")

  @publish(name = "core.commaCommute")
  val commaCommute: ProofInfo = ProofInfo.coreAxiom(", commute")

  @publish(name = "core.DX")
  val DX: ProofInfo = ProofInfo.coreAxiom("DX differential skip")

  @publish(name = "core.Dcomp")
  val Dcomp: ProofInfo = ProofInfo.coreAxiom("D[;] differential self compose")

  @publish(name = "core.DIogreater")
  val DIogreater: ProofInfo = ProofInfo.coreAxiom("DIo open differential invariance >")

  @publish(name = "core.DMP")
  val DMP: ProofInfo = ProofInfo.coreAxiom("DMP differential modus ponens")

  @publish(name = "core.Uniq")
  val Uniq: ProofInfo = ProofInfo.coreAxiom("Uniq uniqueness")

  @publish(name = "core.Cont")
  val Cont: ProofInfo = ProofInfo.coreAxiom("Cont continuous existence")

  @publish(name = "core.RIclosedgeq")
  val RIclosedgeq: ProofInfo = ProofInfo.coreAxiom("RI& closed real induction >=")

  @publish(name = "core.RI")
  val RI: ProofInfo = ProofInfo.coreAxiom("RI& real induction")

  @publish(name = "core.IVT")
  val IVT: ProofInfo = ProofInfo.coreAxiom("IVT")

  @publish(name = "core.DCC")
  val DCC: ProofInfo = ProofInfo.coreAxiom("DCC")

  @publish(name = "core.Dconst")
  val Dconst: ProofInfo = ProofInfo.coreAxiom("c()' derive constant fn")

  @publish(name = "core.DvarAxiom")
  val DvarAxiom: ProofInfo = ProofInfo.coreAxiom("x' derive var")

  @publish(name = "core.Dneg")
  val Dneg: ProofInfo = ProofInfo.coreAxiom("-' derive neg")

  @publish(name = "core.Dplus")
  val Dplus: ProofInfo = ProofInfo.coreAxiom("+' derive sum")

  @publish(name = "core.Dminus")
  val Dminus: ProofInfo = ProofInfo.coreAxiom("-' derive minus")

  @publish(name = "core.Dtimes")
  val Dtimes: ProofInfo = ProofInfo.coreAxiom("*' derive product")

  @publish(name = "core.Dquotient")
  val Dquotient: ProofInfo = ProofInfo.coreAxiom("/' derive quotient")

  @publish(name = "core.Dcompose")
  val Dcompose: ProofInfo = ProofInfo.coreAxiom("chain rule")

  @publish(name = "core.Dpower")
  val Dpower: ProofInfo = ProofInfo.coreAxiom("^' derive power")

  @publish(name = "core.Dgreaterequal")
  val Dgreaterequal: ProofInfo = ProofInfo.coreAxiom(">=' derive >=")

  @publish(name = "core.Dgreater")
  val Dgreater: ProofInfo = ProofInfo.coreAxiom(">' derive >")

  @publish(name = "core.Dand")
  val Dand: ProofInfo = ProofInfo.coreAxiom("&' derive and")

  @publish(name = "core.Dor")
  val Dor: ProofInfo = ProofInfo.coreAxiom("|' derive or")

  @publish(name = "core.Dforall")
  val Dforall: ProofInfo = ProofInfo.coreAxiom("forall' derive forall")

  @publish(name = "core.Dexists")
  val Dexists: ProofInfo = ProofInfo.coreAxiom("exists' derive exists")

  @publish(name = "core.duald")
  val duald: ProofInfo = ProofInfo.coreAxiom("<d> dual")

  @publish(name = "core.VK")
  val VK: ProofInfo = ProofInfo.coreAxiom("VK vacuous")

  @publish(name = "core.boxTrueAxiom")
  val boxTrueAxiom: ProofInfo = ProofInfo.coreAxiom("[]T system")

  @publish(name = "core.K")
  val K: ProofInfo = ProofInfo.coreAxiom("K modal modus ponens")

  @publish(name = "core.Iind")
  val Iind: ProofInfo = ProofInfo.coreAxiom("I induction")

  @publish(name = "core.alld")
  val alld: ProofInfo = ProofInfo.coreAxiom("all dual")

  @publish(name = "core.allPd")
  val allPd: ProofInfo = ProofInfo.coreAxiom("all prime dual")

  @publish(name = "core.alle")
  val alle: ProofInfo = ProofInfo.coreAxiom("all eliminate")

  @publish(name = "core.alleprime")
  val alleprime: ProofInfo = ProofInfo.coreAxiom("all eliminate prime")
}
