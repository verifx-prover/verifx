import org.scalatest.FlatSpec

class RegisterImineTest extends FlatSpec with Prover {
  // Proofs for Imine's transformation functions
  "RegisterImine" should "reject C1" in {
    val imine = ("RegisterImine", "C1")
    val res = reject(imine)
    res
  }

  it should "guarantee C2" in {
    val imine = ("RegisterImine", "C2")
    prove(imine)
  }

  "RegisterImineV2" should "guarantee C1" in {
    val imine = ("RegisterImineV2", "C1")
    val res = prove(imine)
    res
  }

  it should "reject C2" in {
    val imine = ("RegisterImineV2", "C2")
    val res = reject(imine)
    res
  }

  "RegisterImineV3" should "guarantee C1" in {
    val imine = ("RegisterImineV3", "C1")
    val res = prove(imine)
    res
  }

  it should "guarantee C2" in {
    val imine = ("RegisterImineV3", "C2")
    prove(imine)
  }
}
