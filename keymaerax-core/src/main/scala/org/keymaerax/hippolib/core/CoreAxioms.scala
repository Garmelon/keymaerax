/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.hippolochos.proof.HippoProof

/** Axioms from [[org.keymaerax.core.Provable.axioms]]. */
// TODO Check if the list is complete
// TODO Doc comments for every axiom
object CoreAxioms {
  val diamond: HippoProof.CoreAxiom = HippoProof.CoreAxiom("<> diamond")
  val assignbAxiom: HippoProof.CoreAxiom = HippoProof.CoreAxiom("[:=] assign")
  val assignbeq: HippoProof.CoreAxiom = HippoProof.CoreAxiom("[:=] assign equality")
  val selfassignb: HippoProof.CoreAxiom = HippoProof.CoreAxiom("[:=] self assign")
  val Dassignb: HippoProof.CoreAxiom = HippoProof.CoreAxiom("[':=] differential assign")
  val Dassignbeq: HippoProof.CoreAxiom = HippoProof.CoreAxiom("[':=] assign equality")
  val Dselfassignb: HippoProof.CoreAxiom = HippoProof.CoreAxiom("[':=] self assign")
  val randomb: HippoProof.CoreAxiom = HippoProof.CoreAxiom("[:*] assign nondet")
  val testb: HippoProof.CoreAxiom = HippoProof.CoreAxiom("[?] test")
  val choiceb: HippoProof.CoreAxiom = HippoProof.CoreAxiom("[++] choice")
  val composeb: HippoProof.CoreAxiom = HippoProof.CoreAxiom("[;] compose")
  val iterateb: HippoProof.CoreAxiom = HippoProof.CoreAxiom("[*] iterate")
  val barcan: HippoProof.CoreAxiom = HippoProof.CoreAxiom("B Barcan")
  val DWbase: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DW base")
  val DE: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DE differential effect")
  val DEs: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DE differential effect (system)")
  val DIequiv: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DI differential invariance")
  val DGa: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DG differential ghost")
  val DGpp: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DG inverse differential ghost")
  val DGi: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DG inverse differential ghost implicational")
  val DGC: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DG differential ghost constant")
  val DGCa: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DG differential ghost constant all")
  val DS: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DS& differential equation solution")
  val commaSort: HippoProof.CoreAxiom = HippoProof.CoreAxiom(", sort")
  val commaCommute: HippoProof.CoreAxiom = HippoProof.CoreAxiom(", commute")
  val DX: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DX differential skip")
  val Dcomp: HippoProof.CoreAxiom = HippoProof.CoreAxiom("D[;] differential self compose")
  val DIogreater: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DIo open differential invariance >")
  val DMP: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DMP differential modus ponens")
  val Uniq: HippoProof.CoreAxiom = HippoProof.CoreAxiom("Uniq uniqueness")
  val Cont: HippoProof.CoreAxiom = HippoProof.CoreAxiom("Cont continuous existence")
  val RIclosedgeq: HippoProof.CoreAxiom = HippoProof.CoreAxiom("RI& closed real induction >=")
  val RI: HippoProof.CoreAxiom = HippoProof.CoreAxiom("RI& real induction")
  val IVT: HippoProof.CoreAxiom = HippoProof.CoreAxiom("IVT")
  val DCC: HippoProof.CoreAxiom = HippoProof.CoreAxiom("DCC")
  val Dconst: HippoProof.CoreAxiom = HippoProof.CoreAxiom("c()' derive constant fn")
  val DvarAxiom: HippoProof.CoreAxiom = HippoProof.CoreAxiom("x' derive var")
  val Dneg: HippoProof.CoreAxiom = HippoProof.CoreAxiom("-' derive neg")
  val Dplus: HippoProof.CoreAxiom = HippoProof.CoreAxiom("+' derive sum")
  val Dminus: HippoProof.CoreAxiom = HippoProof.CoreAxiom("-' derive minus")
  val Dtimes: HippoProof.CoreAxiom = HippoProof.CoreAxiom("*' derive product")
  val Dquotient: HippoProof.CoreAxiom = HippoProof.CoreAxiom("/' derive quotient")
  val Dcompose: HippoProof.CoreAxiom = HippoProof.CoreAxiom("chain rule")
  val Dpower: HippoProof.CoreAxiom = HippoProof.CoreAxiom("^' derive power")
  val Dgreaterequal: HippoProof.CoreAxiom = HippoProof.CoreAxiom(">=' derive >=")
  val Dgreater: HippoProof.CoreAxiom = HippoProof.CoreAxiom(">' derive >")
  val Dand: HippoProof.CoreAxiom = HippoProof.CoreAxiom("&' derive and")
  val Dor: HippoProof.CoreAxiom = HippoProof.CoreAxiom("|' derive or")
  val Dforall: HippoProof.CoreAxiom = HippoProof.CoreAxiom("forall' derive forall")
  val Dexists: HippoProof.CoreAxiom = HippoProof.CoreAxiom("exists' derive exists")
  val duald: HippoProof.CoreAxiom = HippoProof.CoreAxiom("<d> dual")
  val VK: HippoProof.CoreAxiom = HippoProof.CoreAxiom("VK vacuous")
  val boxTrueAxiom: HippoProof.CoreAxiom = HippoProof.CoreAxiom("[]T system")
  val K: HippoProof.CoreAxiom = HippoProof.CoreAxiom("K modal modus ponens")
  val Iind: HippoProof.CoreAxiom = HippoProof.CoreAxiom("I induction")
  val alld: HippoProof.CoreAxiom = HippoProof.CoreAxiom("all dual")
  val allPd: HippoProof.CoreAxiom = HippoProof.CoreAxiom("all prime dual")
  val alle: HippoProof.CoreAxiom = HippoProof.CoreAxiom("all eliminate")
  val alleprime: HippoProof.CoreAxiom = HippoProof.CoreAxiom("all eliminate prime")
}
