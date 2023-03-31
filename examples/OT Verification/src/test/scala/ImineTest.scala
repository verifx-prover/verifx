import org.scalatest.FlatSpec

class ImineTest extends FlatSpec with Prover {
  // Proofs for Imine's transformation functions
  "Imine" should "guarantee C1 for inserts" in {
    val imine = ("Imine", "C1_inserts")
    prove(imine)
  }

  it should "guarantee C1 for deletes" in {
    val imine = ("Imine", "C1_deletes")
    prove(imine)
  }

  it should "guarantee C1 for mix1" in {
    val imine = ("Imine", "C1_mix1")
    prove(imine)
  }

  it should "guarantee C1 for mix2" in {
    val imine = ("Imine", "C1_mix2")
    prove(imine)
  }

  it should "violate C2" in {
    val imine = ("Imine", "C2")
    reject(imine)
  }

  it should "compatible commute" in {
    val imine = ("Imine", "compatibleCommutes")
    prove(imine)
  }
}
