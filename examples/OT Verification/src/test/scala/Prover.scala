import org.verifx.Analysis.Proofs.{Aborted, ProofName, ProofResult, Proved, Rejected}
import org.verifx.Compiler.ProjectCompiler
import org.verifx.Utilities.Scanner

trait Prover {
  val files = Scanner.scan().toSet
  val pc = ProjectCompiler(files)

  def prove(proof: ProofName, timeout: Int = 10000): ProofResult = {
    val proofRes = pc.checkProof(proof, 5, timeout)
    val reason = proofRes match {
      case _: Rejected => "rejected"
      case _: Aborted => "aborted"
      case _ => "proved"
    }
    assert(proofRes.isInstanceOf[Proved], s"Could not prove $proof [$reason]")
    proofRes
  }

  def reject(proof: ProofName, timeout: Int = 10000): ProofResult = {
    val proofRes = pc.checkProof(proof, 5, timeout)
    val reason = proofRes match {
      case _: Rejected => "rejected"
      case _: Aborted => "aborted"
      case _ => "proved"
    }
    assert(proofRes.isInstanceOf[Rejected], s"Could not reject $proof [$reason]")
    proofRes
  }

  def rejectForModel(proof: ProofName): ProofResult = {
    val proofRes = pc.checkProofForModel(proof)
    val reason = proofRes match {
      case _: Rejected => "rejected"
      case _: Aborted => "aborted"
      case _ => "proved"
    }
    assert(proofRes.isInstanceOf[Rejected], s"Could not reject $proof [$reason]")
    proofRes
  }
}
