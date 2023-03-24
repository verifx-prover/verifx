package be.vub.kdeporre.verifx

import be.vub.kdeporre.verifx.Compiler.ProjectCompiler
import be.vub.kdeporre.verifx.Utilities.Scanner

/**
 * Scans the `src/main/verifx/` directory for VeriFx files,
 * runs all proofs defined in those files
 * and prints the results to the console.
 */
object ProofRunner extends App {
  val vfxFiles = Scanner.scan.toSet
  val compiler: ProjectCompiler = ProjectCompiler(vfxFiles)
  compiler.printProofs()
}
