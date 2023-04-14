package org.verifx.verifx.Analysis

import org.verifx.verifx.Compiler.Plugins.Z3CompilerPlugin
import org.verifx.verifx.Compiler.Plugins.Z3CompilerPlugin.Z3Type
import org.verifx.verifx.Compiler.Types.{Classes, Proof}
import com.microsoft.z3.{Context, Model, Solver}
import org.verifx.verifx.Compiler.Plugins.Z3CompilerPlugin.{CompilerState, Z3Type}
import org.verifx.verifx.Compiler.Types.{Classes, Proof}

import scala.collection.SortedSet

object Proofs {
  /**
   * Identifies a proof.
   * The first element of the tuple is the object in which the proof is defined,
   * the second element is the name of the proof.
   */
  type ProofName = (String, String)

  /**
   * Marker trait for proof results.
   */
  sealed trait ProofResult {
    val z3Program: String
    def dispose(): Unit = ()
  }

  /**
   * The goal was succesfully proven.
   */
  final case class Proved(z3Program: String) extends ProofResult

  /**
   * Found a counter example, hence, the proof was rejected.
   */
  final case class Rejected(vars: List[Variable], types: List[TypeParamDefinition], z3Program: String, varsAndType: List[(String, Z3Type)], solver: Solver, z3: Context) extends ProofResult {
    def counterExample() = {
      (vars, types)
    }

    override def toString(): String = {
      val typesStr = types.mkString("\n")
      val varsStr = vars.mkString("\n")
      s"Type definitions:\n\n$typesStr\n\nVariable assignments:\n\n$varsStr"
    }

    /**
     * Disposes the memory that is used by the current proof.
     * This is needed in order to free the memory used by the underlying SMT solver.
     * Do not call this method more than once.
     */
    override def dispose(): Unit = z3.close()

    // TODO: we could now immediately call dispose() since we queried and reconstructed the model
  }

  def mkRejected(z3Program: String, varsAndType: List[(String, Z3Type)], solver: Solver, z3: Context): Rejected = {
    import ModelReconstructor._
    val ex = solver.getModel()
    val reconstructedVariables = reconstructVars(varsAndType.toMap)(ex, z3)
    val sortedVariables = reconstructedVariables.sortWith((var1, var2) => varsAndType.indexWhere(_._1.endsWith(var1.name)) < varsAndType.indexWhere(_._1.endsWith(var2.name)))
    val reconstructedSorts = reconstructUninterpretedSorts()(ex, z3)
    Rejected(sortedVariables, reconstructedSorts, z3Program, varsAndType, solver, z3)
  }

  /**
   * The proof was aborted, e.g. because of a timeout.
   */
  final case class Aborted(z3Program: String) extends ProofResult

  /**
    * Returns a tuple containing the proof program and the list of universally quantified variables and their types
    **/
  def getProofProgram(proof: Proof, z3Program: String, classTable: Classes, z3CompilerState: CompilerState, maxTries: Int = 5, timeout: Int = 10000, eliminateForall: Boolean = false): (String, List[(String, Z3Type)]) = proof match {
    case Proof(proofName, _, _, dummyClassName) => {
      val name = if (!eliminateForall) proofName else s"${proofName}_eliminated"

      val dummyClazz = classTable.classTable.get(dummyClassName).get

      val z3TypeParams = dummyClazz.tparams.map(tparam => {
        val tparamType = Z3CompilerPlugin.Z3Type(tparam.name.value, Nil)
        Z3CompilerPlugin.Z3Type(dummyClassName, List(tparamType))
      })

      val proofFunction = Z3CompilerPlugin.makeFunName(dummyClassName, z3TypeParams, name, Nil)

      // Fetch the Z3 method for this proof
      val z3ProofClass = z3CompilerState.classes.get(dummyClassName).get
      val proofMethod = z3ProofClass.methods.get(name).get.method
      val args = proofMethod.args

      val paramNamesAndType = args.map(param => {
        val name = s"${proofFunction}_${param.name}"
        val tpe = param.tpe
        (name, tpe)
      })

      val varsAndType = paramNamesAndType.map(nameAndType => (nameAndType._1, nameAndType._2.asInstanceOf[Z3Type]))

      // Declare a constant for each parameter of the proof
      val paramDeclarations = paramNamesAndType.map {
        case (name, tpe) => s"(declare-const $name $tpe)"
      }

      // Call the proof function with those constants
      val callProofFunction =
        if (paramNamesAndType.isEmpty)
          proofFunction
        else
          s"($proofFunction ${paramNamesAndType.map(_._1).mkString(" ")})"

      val proofProgram =
        s"""$z3Program
           |${paramDeclarations.mkString("\n")}
           |
           |; Goal to prove
           |(assert
           |  (not $callProofFunction))""".stripMargin

      (proofProgram, varsAndType)
    }
  }

  def checkProof(proof: Proof, z3Program: String, classTable: Classes, z3CompilerState: Z3CompilerPlugin.CompilerState, maxTries: Int = 5, timeout: Int = 10000): ProofResult = {
    val proofProgram = getProofProgram(proof, z3Program, classTable, z3CompilerState, maxTries, timeout, false)._1
    val res = checkProofInternal(proofProgram, maxTries, timeout)
    res.dispose() // free the memory used by Z3 since we will not use the model

    res match {
      case abortedRes @ Aborted(_) => {
        // Try again by eliminating the top-level negated forall
        val dummyClassName = proof.dummyClassName
        val z3ProofClass = z3CompilerState.classes.get(dummyClassName).get
        val proofNameWithoutForall = s"${proof.name}_eliminated"

        if (z3ProofClass.methods.contains(proofNameWithoutForall)) {
          println(s"Retrying proof ${proof.name} without top-level negated forall")
          val newRes = checkProofForModel(proof, z3Program, classTable, z3CompilerState, maxTries, timeout)
          newRes.dispose() // free the memory used by Z3 since we will not use the model
          newRes
        }
        else {
          abortedRes
        }
      }
      case _ => res
    }
  }

  private def checkProofForModelInternal(proof: Proof, z3Program: String, classTable: Classes, z3CompilerState: Z3CompilerPlugin.CompilerState, maxTries: Int = 5, timeout: Int = 10000): ProofResult = {
    val (proofProgram, varsAndType) = getProofProgram(proof, z3Program, classTable, z3CompilerState, maxTries, timeout, true)
    checkProofInternal(proofProgram, maxTries, timeout, varsAndType)
  }

  // If there is a top-level forall it will eliminate it to get a more meaningful counterexample.
  // If not, it will just check the given proof.
  /**
   * Checks if the proof holds and returns a counter example if that is not the case.
   * Do not forget to call `dispose()` on the returned [[ProofResult]] !
   */
  def checkProofForModel(proof: Proof, z3Program: String, classTable: Classes, z3CompilerState: Z3CompilerPlugin.CompilerState, maxTries: Int = 5, timeout: Int = 10000): ProofResult = {
    val dummyClassName = proof.dummyClassName
    val z3ProofClass = z3CompilerState.classes.get(dummyClassName).get
    val proofNameWithoutForall = s"${proof.name}_eliminated"

    if (z3ProofClass.methods.contains(proofNameWithoutForall)) {
      checkProofForModelInternal(proof, z3Program, classTable, z3CompilerState, maxTries, timeout)
    }
    else {
      // Just check the normal proof (don't call checkProof because it will dispose the ProofResult)
      val proofProgram = getProofProgram(proof, z3Program, classTable, z3CompilerState, maxTries, timeout, false)._1
      checkProofInternal(proofProgram, maxTries, timeout)
    }
  }

  private def checkProofInternal(proofProgram: String, maxTries: Int = 5, timeout: Int = 10000, varsAndType: List[(String, Z3Type)] = List()): ProofResult = {
    val prover = new Z3Prover {}
    prover.prove(proofProgram, maxTries, timeout, varsAndType)
  }

  def checkProofs(proofs: Set[Proof], z3Program: String, classTable: Classes, z3CompilerState: Z3CompilerPlugin.CompilerState, maxTries: Int = 5, timeout: Int = 10000): Map[ProofName, ProofResult] = {
    //import scala.collection.parallel.CollectionConverters._

    // Run the proofs in parallel
    proofs.foldLeft(Map.empty[ProofName, ProofResult]) {
      case (results, proof @ Proof(name, objName, _, _)) => {
        val proofResult = checkProof(proof, z3Program, classTable, z3CompilerState, maxTries, timeout)
        results + ((objName, name) -> proofResult)
      }
    }
  }
}
