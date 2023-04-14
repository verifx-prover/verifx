package org.verifx.verifx

import org.verifx.verifx.Compiler.ProjectCompiler
import org.verifx.verifx.Utilities.Scanner

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
