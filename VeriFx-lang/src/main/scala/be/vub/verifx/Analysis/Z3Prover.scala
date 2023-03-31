package be.vub.verifx.Analysis

import java.util.HashMap
import be.vub.verifx.Analysis.Proofs.{Aborted, ProofResult, Proved, Rejected}
import be.vub.verifx.Compiler.Plugins.Z3CompilerPlugin.Z3Type
import be.vub.verifx.Compiler.Plugins.Z3CompilerPlugin.Z3Type
import com.microsoft.z3.{Context, Solver}

trait Z3Prover {
  private def proveInternal(strZ3: String)(implicit maxTries: Int, timeout: Int): (String, Solver, Context) = {
    try {
      val cfg: HashMap[String, String] = new HashMap[String, String]()
      cfg.put("model", "true") // required if you plan to query models of satisfiable constraints
      val z3 = new Context(cfg)
      //val z3 = new Context()
      var solver: Solver = null
      var sat = "UNKNOWN"
      var tries = 0

      /*
       * Z3 uses random seeds when proving goals involving the theory of arrays.
       * Depending on the seed it may or may not prove the goal.
       * When Z3 gets stuck trying to prove it, it just gives up and returns "UNKNOWN".
       * Using another seed on the same program, Z3 may be able to prove the same goal!
       * Therefore, if Z3 returns "UNKNOWN" we will try again, up to a maximum of `maxTries` retries.
       * For more information see: https://github.com/Z3Prover/z3/issues/930
       *
       * For the same reason, Z3 sometimes "hangs", in fact it just chose a path that will take very long to prove (or maybe even an endless path?).
       * So, to handle these exceptional cases, we work with timeouts.
       * If the provided `timeout` is reached, Z3 returns "UNKNOWN" and we retry up to a maximum of `maxTries` times.
       */
      while (sat == "UNKNOWN" && tries < maxTries) {
        tries = tries + 1

        val parsed = z3.parseSMTLIB2String(strZ3, null, null, null, null)
        val exp = z3.mkAnd(parsed:_*)
        solver = z3.mkSolver()

        // Add the timeout
        val p = z3.mkParams()
        p.add("timeout", timeout)
        solver.setParameters(p)

        // Prove the program
        solver.add(exp)
        val res = solver.check()
        sat = res.name()
      }

      //solver.reset()
      //z3.close()

      (sat, solver, z3)
    } catch {
      case e: Throwable => throw e
    }
  }

  /**
   * Tries to prove the given Z3 program.
   * @param strZ3 The Z3 program
   * @param maxTries Maximum number of retries
   * @param timeout Timeout for the analysis in milliseconds
   * @return A [[ProofResult]] object representing the result of the proof
   */
  def prove(strZ3: String, maxTries: Int = 5, timeout: Int = 10000, varsAndType: List[(String, Z3Type)] = List()): ProofResult = {
    val (sat, solver, z3) = proveInternal(strZ3)(maxTries, timeout)
    val res = sat match {
      case "UNSATISFIABLE" => {
        z3.close() // disposes the Z3 Context
        Proved(strZ3)
      }
      case "SATISFIABLE" => {
        // In order for the programmer to be able to use the returned model, the Z3 context may not be closed yet.
        // IT IS THE PROGRAMMER'S RESPONSIBILITY TO CALL "dispose" ON THE PROOF RESULT!
        Proofs.mkRejected(strZ3, varsAndType, solver, z3)
      }
      case "UNKNOWN" => {
        z3.close() // disposes the Z3 Context
        Aborted(strZ3)
      }
      case str => throw new InternalError(s"Unexpected answer from Z3: $str")
    }

    res
  }
}
