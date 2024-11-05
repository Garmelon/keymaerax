/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core
import org.keymaerax.core.hippolib.publish
import org.keymaerax.hippolib.meta.{ProofInfo, TacticArg, TacticArgInfo, TacticConstructor, TacticInfo}
import org.keymaerax.hippolib.{core => self, HippoLib}
import org.keymaerax.hippolochos.run.HippoContext

/**
 * A collection of core axioms, axiomatic rules, and tactics.
 *
 * Core axioms are the axioms from [[org.keymaerax.core.Provable.axioms]]. Core axiomatic rules are the rules from
 * [[org.keymaerax.core.Provable.rules]].
 */
class Lib(implicit ctx: HippoContext, lib: HippoLib) {
  /////////////////
  // Core axioms //
  /////////////////

  @publish(name = "core.diamond")
  val diamond: ProofInfo = ProofInfo { ctx.coreAxiom("<> diamond") }

  @publish(name = "core.assignbAxiom")
  val assignbAxiom: ProofInfo = ProofInfo { ctx.coreAxiom("[:=] assign") }

  @publish(name = "core.assignbeq")
  val assignbeq: ProofInfo = ProofInfo { ctx.coreAxiom("[:=] assign equality") }

  @publish(name = "core.selfassignb")
  val selfassignb: ProofInfo = ProofInfo { ctx.coreAxiom("[:=] self assign") }

  @publish(name = "core.Dassignb")
  val Dassignb: ProofInfo = ProofInfo { ctx.coreAxiom("[':=] differential assign") }

  @publish(name = "core.Dassignbeq")
  val Dassignbeq: ProofInfo = ProofInfo { ctx.coreAxiom("[':=] assign equality") }

  @publish(name = "core.Dselfassignb")
  val Dselfassignb: ProofInfo = ProofInfo { ctx.coreAxiom("[':=] self assign") }

  @publish(name = "core.randomb")
  val randomb: ProofInfo = ProofInfo { ctx.coreAxiom("[:*] assign nondet") }

  @publish(name = "core.testb")
  val testb: ProofInfo = ProofInfo { ctx.coreAxiom("[?] test") }

  @publish(name = "core.choiceb")
  val choiceb: ProofInfo = ProofInfo { ctx.coreAxiom("[++] choice") }

  @publish(name = "core.composeb")
  val composeb: ProofInfo = ProofInfo { ctx.coreAxiom("[;] compose") }

  @publish(name = "core.iterateb")
  val iterateb: ProofInfo = ProofInfo { ctx.coreAxiom("[*] iterate") }

  @publish(name = "core.barcan")
  val barcan: ProofInfo = ProofInfo { ctx.coreAxiom("B Barcan") }

  @publish(name = "core.DWbase")
  val DWbase: ProofInfo = ProofInfo { ctx.coreAxiom("DW base") }

  @publish(name = "core.DE")
  val DE: ProofInfo = ProofInfo { ctx.coreAxiom("DE differential effect") }

  @publish(name = "core.DEs")
  val DEs: ProofInfo = ProofInfo { ctx.coreAxiom("DE differential effect (system)") }

  @publish(name = "core.DIequiv")
  val DIequiv: ProofInfo = ProofInfo { ctx.coreAxiom("DI differential invariance") }

  @publish(name = "core.DGa")
  val DGa: ProofInfo = ProofInfo { ctx.coreAxiom("DG differential ghost") }

  @publish(name = "core.DGpp")
  val DGpp: ProofInfo = ProofInfo { ctx.coreAxiom("DG inverse differential ghost") }

  @publish(name = "core.DGi")
  val DGi: ProofInfo = ProofInfo { ctx.coreAxiom("DG inverse differential ghost implicational") }

  @publish(name = "core.DGC")
  val DGC: ProofInfo = ProofInfo { ctx.coreAxiom("DG differential ghost constant") }

  @publish(name = "core.DGCa")
  val DGCa: ProofInfo = ProofInfo { ctx.coreAxiom("DG differential ghost constant all") }

  @publish(name = "core.DS")
  val DS: ProofInfo = ProofInfo { ctx.coreAxiom("DS& differential equation solution") }

  @publish(name = "core.commaSort")
  val commaSort: ProofInfo = ProofInfo { ctx.coreAxiom(", sort") }

  @publish(name = "core.commaCommute")
  val commaCommute: ProofInfo = ProofInfo { ctx.coreAxiom(", commute") }

  @publish(name = "core.DX")
  val DX: ProofInfo = ProofInfo { ctx.coreAxiom("DX differential skip") }

  @publish(name = "core.Dcomp")
  val Dcomp: ProofInfo = ProofInfo { ctx.coreAxiom("D[;] differential self compose") }

  @publish(name = "core.DIogreater")
  val DIogreater: ProofInfo = ProofInfo { ctx.coreAxiom("DIo open differential invariance >") }

  @publish(name = "core.DMP")
  val DMP: ProofInfo = ProofInfo { ctx.coreAxiom("DMP differential modus ponens") }

  @publish(name = "core.Uniq")
  val Uniq: ProofInfo = ProofInfo { ctx.coreAxiom("Uniq uniqueness") }

  @publish(name = "core.Cont")
  val Cont: ProofInfo = ProofInfo { ctx.coreAxiom("Cont continuous existence") }

  @publish(name = "core.RIclosedgeq")
  val RIclosedgeq: ProofInfo = ProofInfo { ctx.coreAxiom("RI& closed real induction >=") }

  @publish(name = "core.RI")
  val RI: ProofInfo = ProofInfo { ctx.coreAxiom("RI& real induction") }

  @publish(name = "core.IVT")
  val IVT: ProofInfo = ProofInfo { ctx.coreAxiom("IVT") }

  @publish(name = "core.DCC")
  val DCC: ProofInfo = ProofInfo { ctx.coreAxiom("DCC") }

  @publish(name = "core.Dconst")
  val Dconst: ProofInfo = ProofInfo { ctx.coreAxiom("c()' derive constant fn") }

  @publish(name = "core.DvarAxiom")
  val DvarAxiom: ProofInfo = ProofInfo { ctx.coreAxiom("x' derive var") }

  @publish(name = "core.Dneg")
  val Dneg: ProofInfo = ProofInfo { ctx.coreAxiom("-' derive neg") }

  @publish(name = "core.Dplus")
  val Dplus: ProofInfo = ProofInfo { ctx.coreAxiom("+' derive sum") }

  @publish(name = "core.Dminus")
  val Dminus: ProofInfo = ProofInfo { ctx.coreAxiom("-' derive minus") }

  @publish(name = "core.Dtimes")
  val Dtimes: ProofInfo = ProofInfo { ctx.coreAxiom("*' derive product") }

  @publish(name = "core.Dquotient")
  val Dquotient: ProofInfo = ProofInfo { ctx.coreAxiom("/' derive quotient") }

  @publish(name = "core.Dcompose")
  val Dcompose: ProofInfo = ProofInfo { ctx.coreAxiom("chain rule") }

  @publish(name = "core.Dpower")
  val Dpower: ProofInfo = ProofInfo { ctx.coreAxiom("^' derive power") }

  @publish(name = "core.Dgreaterequal")
  val Dgreaterequal: ProofInfo = ProofInfo { ctx.coreAxiom(">=' derive >=") }

  @publish(name = "core.Dgreater")
  val Dgreater: ProofInfo = ProofInfo { ctx.coreAxiom(">' derive >") }

  @publish(name = "core.Dand")
  val Dand: ProofInfo = ProofInfo { ctx.coreAxiom("&' derive and") }

  @publish(name = "core.Dor")
  val Dor: ProofInfo = ProofInfo { ctx.coreAxiom("|' derive or") }

  @publish(name = "core.Dforall")
  val Dforall: ProofInfo = ProofInfo { ctx.coreAxiom("forall' derive forall") }

  @publish(name = "core.Dexists")
  val Dexists: ProofInfo = ProofInfo { ctx.coreAxiom("exists' derive exists") }

  @publish(name = "core.duald")
  val duald: ProofInfo = ProofInfo { ctx.coreAxiom("<d> dual") }

  @publish(name = "core.VK")
  val VK: ProofInfo = ProofInfo { ctx.coreAxiom("VK vacuous") }

  @publish(name = "core.boxTrueAxiom")
  val boxTrueAxiom: ProofInfo = ProofInfo { ctx.coreAxiom("[]T system") }

  @publish(name = "core.K")
  val K: ProofInfo = ProofInfo { ctx.coreAxiom("K modal modus ponens") }

  @publish(name = "core.Iind")
  val Iind: ProofInfo = ProofInfo { ctx.coreAxiom("I induction") }

  @publish(name = "core.alld")
  val alld: ProofInfo = ProofInfo { ctx.coreAxiom("all dual") }

  @publish(name = "core.allPd")
  val allPd: ProofInfo = ProofInfo { ctx.coreAxiom("all prime dual") }

  @publish(name = "core.alle")
  val alle: ProofInfo = ProofInfo { ctx.coreAxiom("all eliminate") }

  @publish(name = "core.alleprime")
  val alleprime: ProofInfo = ProofInfo { ctx.coreAxiom("all eliminate prime") }

  //////////////////////////
  // Core axiomatic rules //
  //////////////////////////

  @publish(name = "core.CQrule")
  val CQrule: ProofInfo = ProofInfo { ctx.coreAxiomaticRule("CQ equation congruence") }

  @publish(name = "core.CErule")
  val CErule: ProofInfo = ProofInfo { ctx.coreAxiomaticRule("CE congruence") }

  @publish(name = "core.mondrule")
  val mondrule: ProofInfo = ProofInfo { ctx.coreAxiomaticRule("<> monotone") }

  @publish(name = "core.FPrule")
  val FPrule: ProofInfo = ProofInfo { ctx.coreAxiomaticRule("FP fixpoint") }

  @publish(name = "core.conrule")
  val conrule: ProofInfo = ProofInfo { ctx.coreAxiomaticRule("con convergence") }

  /////////////
  // Tactics //
  /////////////

  @publish(name = "core.CEqAt")
  val CEqAt: TacticInfo = TacticInfo("core.CEqAt", TacticArgInfo(name = "at", arg = TacticArg.ExprPath)) { at =>
    self.CEqAt(at)
  }

  @publish(name = "core.Skolemize")
  val Skolemize: TacticInfo = TacticInfo("core.Skolemize", TacticArgInfo("pos", TacticArg.SeqPos)) { pos =>
    CoreRule(core.Skolemize(pos))
  }

  @publish(name = "core.QE")
  val QE: TacticInfo = TacticInfo("core.QE") { self.QE() }

  @publish(name = "core.RewriteAt")
  val RewriteAt: TacticInfo = TacticInfo(
    "core.RewriteAt",
    TacticArgInfo(name = "at", arg = TacticArg.ExprPath),
    TacticArgInfo(name = "dir", arg = TacticArg.Option(TacticArg.String), default = Some(None)),
  ) { (at, dir) => self.RewriteAt(at = at, dir = dir.map(self.RewriteAt.Dir.parse)) }

  @publish(name = "core.RewriteAtU")
  val RewriteAtU: TacticInfo = TacticInfo(
    "core.RewriteAtU",
    TacticArgInfo(name = "at", arg = TacticArg.ExprPath),
    TacticArgInfo(name = "eq", arg = TacticArg.HippoProof),
    TacticArgInfo(name = "dir", arg = TacticArg.Option(TacticArg.String), default = Some(None)),
  ) { (at, eq, dir) => self.RewriteAtU(at = at, eq = eq, dir = dir.map(self.RewriteAt.Dir.parse)) }

  @publish(name = "core.Unify")
  val Unify: TacticInfo = TacticInfo("core.Unify", TacticArgInfo(name = "proof", arg = TacticArg.HippoProof)) { proof =>
    self.Unify(proof)
  }

  @publish(name = "core.US")
  val US: TacticInfo = TacticInfo(
    "core.US",
    TacticArgInfo(name = "subst", arg = TacticArg.Seq(TacticArg.Tuple2(TacticArg.Expression, TacticArg.Expression))),
  ) { substs => self.US(substs: _*) }
}
