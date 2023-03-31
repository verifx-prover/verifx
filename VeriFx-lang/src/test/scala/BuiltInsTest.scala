import java.nio.file.Path

import be.vub.verifx.Analysis.Proofs.Proved
import be.vub.verifx.Compiler.ProgramCompiler
import org.scalatest.FlatSpec

class BuiltInsTest extends FlatSpec {
  val builtInsFile: Path = java.nio.file.Paths.get("src/test/verifx", "BuiltInProofs.vfx")

  "Built-in methods" should "work" in {
    val pc = ProgramCompiler(builtInsFile)
    val res = pc.checkProofs()
    res.values.foreach(proofRes => assert(proofRes.isInstanceOf[Proved]))
  }
}
