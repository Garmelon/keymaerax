/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.hippolib.meta.ProofInfo

/** Axioms from [[org.keymaerax.core.Provable.axioms]]. */
// TODO Check if the list is complete
// TODO Doc comments for every axiom
object CoreAxioms {
  val diamond: ProofInfo = ProofInfo.coreAxiom("<> diamond")
  val assignbAxiom: ProofInfo = ProofInfo.coreAxiom("[:=] assign")
  val assignbeq: ProofInfo = ProofInfo.coreAxiom("[:=] assign equality")
  val selfassignb: ProofInfo = ProofInfo.coreAxiom("[:=] self assign")
  val Dassignb: ProofInfo = ProofInfo.coreAxiom("[':=] differential assign")
  val Dassignbeq: ProofInfo = ProofInfo.coreAxiom("[':=] assign equality")
  val Dselfassignb: ProofInfo = ProofInfo.coreAxiom("[':=] self assign")
  val randomb: ProofInfo = ProofInfo.coreAxiom("[:*] assign nondet")
  val testb: ProofInfo = ProofInfo.coreAxiom("[?] test")
  val choiceb: ProofInfo = ProofInfo.coreAxiom("[++] choice")
  val composeb: ProofInfo = ProofInfo.coreAxiom("[;] compose")
  val iterateb: ProofInfo = ProofInfo.coreAxiom("[*] iterate")
  val barcan: ProofInfo = ProofInfo.coreAxiom("B Barcan")
  val DWbase: ProofInfo = ProofInfo.coreAxiom("DW base")
  val DE: ProofInfo = ProofInfo.coreAxiom("DE differential effect")
  val DEs: ProofInfo = ProofInfo.coreAxiom("DE differential effect (system)")
  val DIequiv: ProofInfo = ProofInfo.coreAxiom("DI differential invariance")
  val DGa: ProofInfo = ProofInfo.coreAxiom("DG differential ghost")
  val DGpp: ProofInfo = ProofInfo.coreAxiom("DG inverse differential ghost")
  val DGi: ProofInfo = ProofInfo.coreAxiom("DG inverse differential ghost implicational")
  val DGC: ProofInfo = ProofInfo.coreAxiom("DG differential ghost constant")
  val DGCa: ProofInfo = ProofInfo.coreAxiom("DG differential ghost constant all")
  val DS: ProofInfo = ProofInfo.coreAxiom("DS& differential equation solution")
  val commaSort: ProofInfo = ProofInfo.coreAxiom(", sort")
  val commaCommute: ProofInfo = ProofInfo.coreAxiom(", commute")
  val DX: ProofInfo = ProofInfo.coreAxiom("DX differential skip")
  val Dcomp: ProofInfo = ProofInfo.coreAxiom("D[;] differential self compose")
  val DIogreater: ProofInfo = ProofInfo.coreAxiom("DIo open differential invariance >")
  val DMP: ProofInfo = ProofInfo.coreAxiom("DMP differential modus ponens")
  val Uniq: ProofInfo = ProofInfo.coreAxiom("Uniq uniqueness")
  val Cont: ProofInfo = ProofInfo.coreAxiom("Cont continuous existence")
  val RIclosedgeq: ProofInfo = ProofInfo.coreAxiom("RI& closed real induction >=")
  val RI: ProofInfo = ProofInfo.coreAxiom("RI& real induction")
  val IVT: ProofInfo = ProofInfo.coreAxiom("IVT")
  val DCC: ProofInfo = ProofInfo.coreAxiom("DCC")
  val Dconst: ProofInfo = ProofInfo.coreAxiom("c()' derive constant fn")
  val DvarAxiom: ProofInfo = ProofInfo.coreAxiom("x' derive var")
  val Dneg: ProofInfo = ProofInfo.coreAxiom("-' derive neg")
  val Dplus: ProofInfo = ProofInfo.coreAxiom("+' derive sum")
  val Dminus: ProofInfo = ProofInfo.coreAxiom("-' derive minus")
  val Dtimes: ProofInfo = ProofInfo.coreAxiom("*' derive product")
  val Dquotient: ProofInfo = ProofInfo.coreAxiom("/' derive quotient")
  val Dcompose: ProofInfo = ProofInfo.coreAxiom("chain rule")
  val Dpower: ProofInfo = ProofInfo.coreAxiom("^' derive power")
  val Dgreaterequal: ProofInfo = ProofInfo.coreAxiom(">=' derive >=")
  val Dgreater: ProofInfo = ProofInfo.coreAxiom(">' derive >")
  val Dand: ProofInfo = ProofInfo.coreAxiom("&' derive and")
  val Dor: ProofInfo = ProofInfo.coreAxiom("|' derive or")
  val Dforall: ProofInfo = ProofInfo.coreAxiom("forall' derive forall")
  val Dexists: ProofInfo = ProofInfo.coreAxiom("exists' derive exists")
  val duald: ProofInfo = ProofInfo.coreAxiom("<d> dual")
  val VK: ProofInfo = ProofInfo.coreAxiom("VK vacuous")
  val boxTrueAxiom: ProofInfo = ProofInfo.coreAxiom("[]T system")
  val K: ProofInfo = ProofInfo.coreAxiom("K modal modus ponens")
  val Iind: ProofInfo = ProofInfo.coreAxiom("I induction")
  val alld: ProofInfo = ProofInfo.coreAxiom("all dual")
  val allPd: ProofInfo = ProofInfo.coreAxiom("all prime dual")
  val alle: ProofInfo = ProofInfo.coreAxiom("all eliminate")
  val alleprime: ProofInfo = ProofInfo.coreAxiom("all eliminate prime")
}
