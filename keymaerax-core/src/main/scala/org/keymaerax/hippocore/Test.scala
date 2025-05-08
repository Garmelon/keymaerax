/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package org.keymaerax.hippocore

import org.keymaerax.btactics.Z3ToolProvider
import org.keymaerax.core.{Formula, PrettyPrinter, Sequent, Skolemize, SuccPos}
import org.keymaerax.hippocore.cache.{HippoProofFsCache, LruCache, ProvableFsCache}
import org.keymaerax.hippocore.proof.{HippoProof, HippoSequent}
import org.keymaerax.hippocore.run.HippoContext
import org.keymaerax.hippocore.tools.ExprPath
import org.keymaerax.hippolib.HippoLib
import org.keymaerax.hippolib.core.{CoreRule, QE, RewriteAt, RewriteAtU, US}
import org.keymaerax.hippolib.primitive.BidiBackward
import org.keymaerax.parser.StringConverter.StringToStringConverter
import org.keymaerax.tools.install.ToolConfiguration
import org.keymaerax.{Configuration, FileConfiguration}

import java.nio.file.Path

object Test {
  def sequent(formula: Formula): Sequent = Sequent(ante = IndexedSeq(), succ = IndexedSeq(formula))
  def sequent(formula: String): Sequent = sequent(formula.asFormula)

  /**
   * {{{
   *    [x:=*;][?x>0;][x:=x+1;]x>1
   *   ----------------------------
   *      [x:=*;?x>0;x:=x+1;]x>1
   * }}}
   */
  def simpleComposition1(implicit ctx: HippoContext, lib: HippoLib): HippoProof = {

    /**
     * {{{
     *                             *
     *              ------------------------------- [;] compose
     *               [a;b;]p(||) <-> [a;][b;]p(||)
     *   ----------------------------------------------------- US
     *    [x:=*;?x>0;x:=x+1;]x>1 <-> [x:=*;][?x>0;x:=x+1;]x>1
     * }}}
     */
    //
    val eq1 = ctx.forwardJoin(
      US(
        "a;".asProgram -> "x:=*;".asProgram,
        "b;".asProgram -> "?x>0;x:=x+1;".asProgram,
        "p(||)".asFormula -> "x>1".asFormula,
      ),
      lib.core.composeb.proof,
    )

    /**
     * {{{
     *                        *
     *         ------------------------------- [;] compose
     *          [a;b;]p(||) <-> [a;][b;]p(||)
     *   ------------------------------------------- US
     *    [?x>0;x:=x+1;]x>1 <-> [?x>0;][x:=x+1;]x>1
     * }}}
     *
     * This time with the chaining API.
     */
    val eq2 = ctx
      .chain(lib.core.composeb.proof)
      .forward(US(
        "a;".asProgram -> "?x>0;".asProgram,
        "b;".asProgram -> "x:=x+1;".asProgram,
        "p(||)".asFormula -> "x>1".asFormula,
      ))
      .proof

    /**
     * Combine everything:
     *
     * {{{
     *    [x:=*;][?x>0;][x:=x+1;]x>1    eq2
     *   ----------------------------------- RewriteAt
     *        [x:=*;][?x>0;x:=x+1;]x>1                    eq1
     *       ------------------------------------------------- RewriteAt
     *                    [x:=*;?x>0;x:=x+1;]x>1
     * }}}
     */
    ctx
      .chain(HippoSequent(Sequent(ante = IndexedSeq(), succ = IndexedSeq("[x:=*;][?x>0;][x:=x+1;]x>1".asFormula))))
      .forwardJoin(RewriteAt(ExprPath(1)), eq2)
      .forwardJoin(RewriteAt(ExprPath()), eq1)
      .proof
  }

  /**
   * {{{
   *    [x:=*;][?x>0;][x:=x+1;]x>1
   *   ---------------------------- RewriteAtU(.1, composeb)
   *     [x:=*;][?x>0;x:=x+1;]x>1
   *    -------------------------- RewriteAtU(., composeb)
   *      [x:=*;?x>0;x:=x+1;]x>1
   * }}}
   */
  def simpleComposition2(implicit ctx: HippoContext, lib: HippoLib): HippoProof = ctx
    .chain(HippoSequent(Sequent(ante = IndexedSeq(), succ = IndexedSeq("[x:=*;][?x>0;][x:=x+1;]x>1".asFormula))))
    .forward(RewriteAtU(ExprPath(1), lib.core.composeb.proof))
    .forward(RewriteAtU(ExprPath(), lib.core.composeb.proof))
    .proof

  /**
   * {{{
   *                  *
   *           --------------- QE
   *            |- x>0->x+1>1
   *        ---------------------- [:=] assign
   *         |- x>0->[x:=x+1;]x>1
   *       ------------------------ [?] test
   *        |– [?x>0;][x:=x+1;]x>1
   *       ------------------------ [;] compose
   *         |– [?x>0;x:=x+1;]x>1
   *   -------------------------------- skolemize
   *    |– \forall x [?x>0;x:=x+1;]x>1
   *   -------------------------------- [:*] assign nondet
   *     |– [x:=*;][?x>0;x:=x+1;]x>1
   *    ----------------------------- [;] compose
   *      |– [x:=*;?x>0;x:=x+1;]x>1
   * }}}
   */
  def simpleComposition3(implicit ctx: HippoContext, lib: HippoLib): HippoProof = ctx
    .chain(HippoSequent("==> [x:=*;?x>0;x:=x+1;]x>1".asSequent))
    .backward(RewriteAtU(ExprPath(), lib.core.composeb.proof))
    .backward(RewriteAtU(ExprPath(), lib.core.randomb.proof))
    .backward(CoreRule(Skolemize(SuccPos(0))))
    .backward(RewriteAtU(ExprPath(), lib.core.composeb.proof))
    .backward(RewriteAtU(ExprPath(), lib.core.testb.proof))
    .backward(RewriteAtU(ExprPath(1), lib.core.assignb.proof))
    .backward(QE())
    .proof

  /**
   * {{{
   *                  *
   *           --------------- QE
   *            |- x>0->x+1>1
   *        ---------------------- [:=] assign
   *         |- x>0->[x:=x+1;]x>1
   *       ------------------------ [?] test
   *        |– [?x>0;][x:=x+1;]x>1
   *       ------------------------ [;] compose
   *         |– [?x>0;x:=x+1;]x>1
   *   -------------------------------- skolemize
   *    |– \forall x [?x>0;x:=x+1;]x>1
   *   -------------------------------- [:*] assign nondet
   *     |– [x:=*;][?x>0;x:=x+1;]x>1
   *    ----------------------------- [;] compose
   *      |– [x:=*;?x>0;x:=x+1;]x>1
   * }}}
   */
  def simpleComposition4(implicit ctx: HippoContext, lib: HippoLib): HippoProof = ctx
    .chain(HippoSequent("==> x>0->x+1>1".asSequent))
    .backward(QE())
    // .forward(Forward(RewriteAt(ExprPath(1)), "==> x>0->[x:=x+1;]x>1".asSequent))
    // .backward(Unify(CoreAxioms.assignbAxiom))
    .forward(BidiBackward(
      RewriteAtU(ExprPath(1), lib.core.assignb.proof),
      HippoSequent("==> x>0->[x:=x+1;]x>1".asSequent),
    ))
    .forward(RewriteAtU(ExprPath(), lib.core.testb.proof))
    .forward(RewriteAtU(ExprPath(), lib.core.composeb.proof))
    .forward(BidiBackward(CoreRule(Skolemize(SuccPos(0))), HippoSequent("==> \\forall x [?x>0;x:=x+1;]x>1".asSequent)))
    .forward(RewriteAtU(ExprPath(), lib.core.randomb.proof))
    .forward(RewriteAtU(ExprPath(), lib.core.composeb.proof))
    .proof

  def main(args: Array[String]): Unit = {
    Configuration.setConfiguration(FileConfiguration)
    PrettyPrinter.setPrinter(org.keymaerax.parser.KeYmaeraXPrettyPrinter.pp)

    val z3ToolProvider = Z3ToolProvider(ToolConfiguration(z3Path = Some("/home/joscha-nixos/stud/keymaerax/z3")))
    z3ToolProvider.init()

    implicit val ctx: HippoContext = new HippoContext(
      toolProvider = z3ToolProvider,
      toolCache =
        new ProvableFsCache(Path.of("/home/joscha-nixos/stud/keymaerax/cache/tool")).behind(new LruCache(1000)),
      tacticCache =
        new HippoProofFsCache(Path.of("/home/joscha-nixos/stud/keymaerax/cache/tactic")).behind(new LruCache(1000)),
      proofCache =
        new ProvableFsCache(Path.of("/home/joscha-nixos/stud/keymaerax/cache/proof")).behind(new LruCache(1000)),
    )

    implicit val lib: HippoLib = new HippoLib

    val proof = simpleComposition3

    printProof(proof)
    println()
    println(proof)

//    val provable = ctx.provableFromLocalProof(proof)
//    println()
//    println(provable)
//
//    val error = SourceFile("first\nsecond\nthird\nfourth\nfifth")
//      .Slice(8, 23)
//      .formatError(error = "oopsie woopsie, something's wrong", label = "probably riiiight here")
//    println()
//    println(error)
  }

  private def printProof(proof: HippoProof): Unit = {
    for (premise <- proof.premises) println(s"${if (premise.mustBeProved) "! " else "  "}${premise.sequent}")
    println("-----------------------------------------")
    println(s"  ${proof.conclusion}")
  }
}
