/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippolib.core

import org.keymaerax.core
import org.keymaerax.core.hippolib.publish
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippolib.meta.{ProofInfo, TacticArg, TacticArgInfo, TacticInfo}
import org.keymaerax.hippolib.{core as self, HippoLib}

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

  @publish(name = "core.assignb")
  val assignb: ProofInfo = ProofInfo { ctx.coreAxiom("[:=] assign") }

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

  ////////////////
  // Core rules //
  ////////////////

  @publish
  val HideRight: TacticInfo = TacticInfo.arg1("core.HideRight", TacticArgInfo("pos", TacticArg.SuccPos)) { pos =>
    CoreRule(core.HideRight(pos))
  }

  @publish
  val HideLeft: TacticInfo = TacticInfo.arg1("core.HideLeft", TacticArgInfo("pos", TacticArg.AntePos)) { pos =>
    CoreRule(core.HideLeft(pos))
  }

  @publish
  val ExchangeRightRule: TacticInfo = TacticInfo.arg2(
    "core.ExchangeRightRule",
    TacticArgInfo("pos1", TacticArg.SuccPos),
    TacticArgInfo("pos2", TacticArg.SuccPos),
  ) { (pos1, pos2) => CoreRule(core.ExchangeRightRule(pos1, pos2)) }

  @publish
  val ExchangeLeftRule: TacticInfo = TacticInfo
    .arg2("core.ExchangeLeftRule", TacticArgInfo("pos1", TacticArg.AntePos), TacticArgInfo("pos2", TacticArg.AntePos)) {
      (pos1, pos2) => CoreRule(core.ExchangeLeftRule(pos1, pos2))
    }

  @publish
  val Close: TacticInfo = TacticInfo
    .arg2("core.Close", TacticArgInfo("assume", TacticArg.AntePos), TacticArgInfo("pos", TacticArg.SuccPos)) {
      (assume, pos) => CoreRule(core.Close(assume, pos))
    }

  @publish
  val CloseTrue: TacticInfo = TacticInfo.arg1("core.CloseTrue", TacticArgInfo("pos", TacticArg.SuccPos)) { pos =>
    CoreRule(core.CloseTrue(pos))
  }

  @publish
  val CloseFalse: TacticInfo = TacticInfo.arg1("core.CloseFalse", TacticArgInfo("pos", TacticArg.AntePos)) { pos =>
    CoreRule(core.CloseFalse(pos))
  }

  @publish
  val Cut: TacticInfo = TacticInfo.arg1("core.Cut", TacticArgInfo("c", TacticArg.Formula)) { c =>
    CoreRule(core.Cut(c))
  }

  @publish
  val NotRight: TacticInfo = TacticInfo.arg1("core.NotRight", TacticArgInfo("pos", TacticArg.SuccPos)) { pos =>
    CoreRule(core.NotRight(pos))
  }

  @publish
  val NotLeft: TacticInfo = TacticInfo.arg1("core.NotLeft", TacticArgInfo("pos", TacticArg.AntePos)) { pos =>
    CoreRule(core.NotLeft(pos))
  }

  @publish
  val AndRight: TacticInfo = TacticInfo.arg1("core.AndRight", TacticArgInfo("pos", TacticArg.SuccPos)) { pos =>
    CoreRule(core.AndRight(pos))
  }

  @publish
  val AndLeft: TacticInfo = TacticInfo.arg1("core.AndLeft", TacticArgInfo("pos", TacticArg.AntePos)) { pos =>
    CoreRule(core.AndLeft(pos))
  }

  @publish
  val OrRight: TacticInfo = TacticInfo.arg1("core.OrRight", TacticArgInfo("pos", TacticArg.SuccPos)) { pos =>
    CoreRule(core.OrRight(pos))
  }

  @publish
  val OrLeft: TacticInfo = TacticInfo.arg1("core.OrLeft", TacticArgInfo("pos", TacticArg.AntePos)) { pos =>
    CoreRule(core.OrLeft(pos))
  }

  @publish
  val ImplyRight: TacticInfo = TacticInfo.arg1("core.ImplyRight", TacticArgInfo("pos", TacticArg.SuccPos)) { pos =>
    CoreRule(core.ImplyRight(pos))
  }

  @publish
  val ImplyLeft: TacticInfo = TacticInfo.arg1("core.ImplyLeft", TacticArgInfo("pos", TacticArg.AntePos)) { pos =>
    CoreRule(core.ImplyLeft(pos))
  }

  @publish
  val EquivRight: TacticInfo = TacticInfo.arg1("core.EquivRight", TacticArgInfo("pos", TacticArg.SuccPos)) { pos =>
    CoreRule(core.EquivRight(pos))
  }

  @publish
  val EquivLeft: TacticInfo = TacticInfo.arg1("core.EquivLeft", TacticArgInfo("pos", TacticArg.AntePos)) { pos =>
    CoreRule(core.EquivLeft(pos))
  }

  @publish
  val UniformRenaming: TacticInfo = TacticInfo.arg2(
    "core.UniformRenaming",
    TacticArgInfo("what", TacticArg.Variable),
    TacticArgInfo("repl", TacticArg.Variable),
  ) { (what, repl) => CoreRule(core.UniformRenaming(what, repl)) }

  @publish
  val BoundRenaming: TacticInfo = TacticInfo.arg3(
    "core.BoundRenaming",
    TacticArgInfo("what", TacticArg.Variable),
    TacticArgInfo("repl", TacticArg.Variable),
    TacticArgInfo("pos", TacticArg.SuccPos),
  ) { (what, repl, pos) => CoreRule(core.BoundRenaming(what, repl, pos)) }

  @publish
  val Skolemize: TacticInfo = TacticInfo.arg1("core.Skolemize", TacticArgInfo("pos", TacticArg.SeqPos)) { pos =>
    CoreRule(core.Skolemize(pos))
  }

  @publish
  val CoHideRight: TacticInfo = TacticInfo.arg1("core.CoHideRight", TacticArgInfo("pos", TacticArg.SuccPos)) { pos =>
    CoreRule(core.CoHideRight(pos))
  }

  @publish
  val CoHideLeft: TacticInfo = TacticInfo.arg1("core.CoHideLeft", TacticArgInfo("pos", TacticArg.AntePos)) { pos =>
    CoreRule(core.CoHideLeft(pos))
  }

  @publish
  val CoHide2: TacticInfo = TacticInfo
    .arg2("core.CoHide2", TacticArgInfo("ante", TacticArg.AntePos), TacticArgInfo("succ", TacticArg.SuccPos)) {
      (ante, succ) => CoreRule(core.CoHide2(ante, succ))
    }

  @publish
  val CutRight: TacticInfo = TacticInfo
    .arg2("core.CutRight", TacticArgInfo("c", TacticArg.Formula), TacticArgInfo("pos", TacticArg.SuccPos)) { (c, pos) =>
      CoreRule(core.CutRight(c, pos))
    }

  @publish
  val CutLeft: TacticInfo = TacticInfo
    .arg2("core.CutLeft", TacticArgInfo("c", TacticArg.Formula), TacticArgInfo("pos", TacticArg.AntePos)) { (c, pos) =>
      CoreRule(core.CutLeft(c, pos))
    }

  @publish
  val CommuteEquivRight: TacticInfo = TacticInfo
    .arg1("core.CommuteEquivRight", TacticArgInfo("pos", TacticArg.SuccPos)) { pos =>
      CoreRule(core.CommuteEquivRight(pos))
    }

  @publish
  val CommuteEquivLeft: TacticInfo = TacticInfo
    .arg1("core.CommuteEquivLeft", TacticArgInfo("pos", TacticArg.AntePos)) { pos =>
      CoreRule(core.CommuteEquivLeft(pos))
    }

  @publish
  val EquivifyRight: TacticInfo = TacticInfo
    .arg1("core.EquivifyRight", TacticArgInfo("pos", TacticArg.SuccPos)) { pos => CoreRule(core.EquivifyRight(pos)) }

  /////////////
  // Tactics //
  /////////////

  @publish
  val CEqAt: TacticInfo = TacticInfo.arg1("core.CEqAt", TacticArgInfo(name = "at", arg = TacticArg.ExprPath)) { at =>
    self.CEqAt(at)
  }

  @publish
  val Hide: TacticInfo = TacticInfo
    .arg1("core.Hide", TacticArgInfo(name = "at", arg = TacticArg.Seq(TacticArg.SeqPos)), vararg = true) { at =>
      self.Hide(at*)
    }

  @publish
  val Keep: TacticInfo = TacticInfo
    .arg1("core.Keep", TacticArgInfo(name = "at", arg = TacticArg.Seq(TacticArg.SeqPos)), vararg = true) { at =>
      self.Keep(at*)
    }

  @publish
  val Expand: TacticInfo = TacticInfo
    .arg1("core.Expand", TacticArgInfo(name = "names", arg = TacticArg.Seq(TacticArg.Name)), vararg = true) { names =>
      self.Expand(names*)
    }

  @publish
  val ExpandAll: TacticInfo = TacticInfo.arg0("core.ExpandAll") { self.ExpandAll }

  @publish
  val Noop: TacticInfo = TacticInfo.arg0("core.Noop") { self.Noop }

  @publish
  val QE: TacticInfo = TacticInfo.arg0("core.QE") { self.QE() }

  @publish
  val RewriteAt: TacticInfo = TacticInfo.arg2(
    "core.RewriteAt",
    TacticArgInfo(name = "at", arg = TacticArg.ExprPath),
    TacticArgInfo(name = "dir", arg = TacticArg.Option(TacticArg.String), default = Some(None)),
  ) { (at, dir) => self.RewriteAt(at = at, dir = dir.map(self.RewriteAt.Dir.parse)) }

  @publish
  val RewriteAtU: TacticInfo = TacticInfo.arg3(
    "core.RewriteAtU",
    TacticArgInfo(name = "at", arg = TacticArg.ExprPath),
    TacticArgInfo(name = "eq", arg = TacticArg.HippoProof),
    TacticArgInfo(name = "dir", arg = TacticArg.Option(TacticArg.String), default = Some(None)),
  ) { (at, eq, dir) => self.RewriteAtU(at = at, eq = eq, dir = dir.map(self.RewriteAt.Dir.parse)) }

  @publish
  val Sorry: TacticInfo = TacticInfo.arg1(
    "core.Sorry",
    TacticArgInfo(name = "conclusion", arg = TacticArg.Option(TacticArg.HippoSequent), default = Some(None)),
  ) { conclusion => self.Sorry(conclusion = conclusion) }

  @publish
  val Unify: TacticInfo = TacticInfo
    .arg1("core.Unify", TacticArgInfo(name = "proof", arg = TacticArg.HippoProof)) { proof => self.Unify(proof) }

  @publish
  val Unpack: TacticInfo = TacticInfo.arg0("core.Unpack") { self.Unpack }

  @publish
  val US: TacticInfo = TacticInfo.arg1(
    "core.US",
    TacticArgInfo(name = "subst", arg = TacticArg.Seq(TacticArg.Tuple2(TacticArg.Expression, TacticArg.Expression))),
    vararg = true,
  ) { substs => self.US(substs: _*) }

  @publish
  val Use: TacticInfo = TacticInfo
    .arg1("core.Use", TacticArgInfo(name = "proof", arg = TacticArg.HippoProof)) { proof => self.Use(proof) }
}
