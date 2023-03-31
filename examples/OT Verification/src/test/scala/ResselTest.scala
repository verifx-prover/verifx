import org.scalatest.FlatSpec

class ResselTest extends FlatSpec with Prover {

  "Ressel" should "guarantee C1 for concurrent inserts" in {
    val ressel = ("Ressel", "C1_inserts")
    prove(ressel)
  }

  it should "guarantee C1 for concurrent deletes" in {
    val ressel = ("Ressel", "C1_deletes")
    prove(ressel)
  }

  it should "guarantee C1 for concurrent insert and delete" in {
    val ressel = ("Ressel", "C1_mix1")
    prove(ressel)
  }

  it should "guarantee C1 for concurrent delete and insert" in {
    val ressel = ("Ressel", "C1_mix2")
    prove(ressel)
  }

  it should "reject C2" in {
    val ressel = ("Ressel", "C2")
    reject(ressel)
  }

  it should "compatible commute" in {
    val ressel = ("Ressel", "compatibleCommutes")
    prove(ressel)
  }
}
