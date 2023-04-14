import org.verifx.Analysis.Proofs.Rejected
import org.scalatest.FlatSpec

class SuleimanTest extends FlatSpec with Prover {
  "Suleiman" should "reject C1 for concurrent inserts" in {
    val suleiman = ("Suleiman", "C1_inserts")
    //prove(suleiman)
    val res = rejectForModel(suleiman)
    println(res)
  }

  it should "guarantee C1 for concurrent deleted" in {
    val suleiman = ("Suleiman", "C1_deletes")
    prove(suleiman)
  }

  it should "guarantee C1 for mix1" in {
    val suleiman = ("Suleiman", "C1_mix1")
    prove(suleiman)
  }

  it should "guarantee C1 for mix2" in {
    val suleiman = ("Suleiman", "C1_mix2")
    prove(suleiman)
  }

  it should "reject C2 for inserts" in {
    val suleiman = ("Suleiman", "C2_inserts")
    val res = rejectForModel(suleiman)
    println(res)
  }

  it should "compatible commute" in {
    val suleiman = ("Suleiman", "compatibleCommutes")
    prove(suleiman)
  }
}
