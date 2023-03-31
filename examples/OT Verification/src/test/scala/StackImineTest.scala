import org.scalatest.FlatSpec

class StackImineTest extends FlatSpec with Prover {
  // Proofs for Imine's transformation functions
  "StackImine" should "reject C1" in {
    val imine = ("StackImineProofs", "C1")
    val res = reject(imine)
    res
  }

  it should "guarantee C2" in {
    val imine = ("StackImineProofs", "C2")
    prove(imine)
  }
}
