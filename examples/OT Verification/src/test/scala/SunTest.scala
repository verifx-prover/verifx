import org.scalatest.FlatSpec

class SunTest extends FlatSpec with Prover {
  // Proofs for Sun's transformation functions

  // Sun does not guarantee C1 for concurrent inserts
  // opI = insert(p1, c1) and opJ = insert(p2, c2) when p1 = p2
  // Counter example:
  //   initial state = "abc"
  //   opI = insert(4, "d")             opJ = insert(4, "e")
  //         |  \          ___________________/  |
  //         v   \_______/________               v
  //       "abcd"      /          \            "abce"
  //         |  insert(5, "e")   insert(5, "d")  |
  //         v                                   v
  //      "abcde"                             "abced"
  "Sun" should "reject C1 for concurrent inserts" in {
    val sun = ("Sun", "C1_inserts")
    reject(sun)
  }

  it should "guarantee C1 for concurrent deletes" in {
    val sun = ("Sun", "C1_deletes")
    prove(sun)
  }

  it should "guarantee C1 for concurrent insert and delete" in {
    val sun = ("Sun", "C1_mix1")
    prove(sun)
  }

  it should "guarantee C1 for concurrent delete and insert" in {
    val sun = ("Sun", "C1_mix2")
    prove(sun)
  }

  it should "be wrong for C2" in {
    val sun = ("Sun", "C2")
    reject(sun)
  }

  it should "compatible commute" in {
    val sun = ("Sun", "compatibleCommutes")
    prove(sun)
  }
}
