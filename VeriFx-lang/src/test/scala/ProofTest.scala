import java.nio.file.Path
import org.verifx.verifx.Analysis.Proofs.{Proved, Rejected}
import org.verifx.verifx.Compiler.ProgramCompiler
import org.scalatest.FlatSpec

class ProofTest extends FlatSpec {
  "Grow-only counter proof" should "be accepted" in {
    val gCounterFile: Path = java.nio.file.Paths.get("src/test/verifx", "GCounter.vfx")
    val pc = ProgramCompiler(gCounterFile)
    val res = pc.checkProofs()
    res.values.foreach(proofRes => assert(proofRes.isInstanceOf[Proved]))
  }

  "Wrong proof" should "be rejected" in {
    val proofFile: Path = java.nio.file.Paths.get("src/test/verifx", "WrongProof.vfx")
    val pc = ProgramCompiler(proofFile)
    val res = pc.checkProofs()
    res.values.foreach(proofRes => assert(proofRes.isInstanceOf[Rejected]))
  }
}
