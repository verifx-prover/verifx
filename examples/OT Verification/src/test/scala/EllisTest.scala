import org.scalatest.FlatSpec

class EllisTest extends FlatSpec with Prover {
  /*
  "Ellisss" should "reject TP1 for mix1" in {
    val ellis = ("Ellis", "C1_mix1")
    reject(ellis, 30000)
  }
  */

  "Ellisss" should "reject TP1 for mix2" in {
    val ellis = ("Ellis", "C1_mix2")
    reject(ellis, 30000)
  }

  it should "guarantee C1 for concurrent inserts" in {
    val ellis = ("Ellis", "C1_inserts")
    prove(ellis)
  }

  it should "guarantee C1 for concurrent deletes" in {
    val ellis = ("Ellis", "C1_deletes")
    prove(ellis)
  }

  it should "prove TP2" in {
    val ellis = ("Ellis", "C2")
    reject(ellis, 30000)
  }

  /*
  "Ellis'" should "reject C1 for mix1" in {
    val ellis = ("Ellis", "C1_mix1")
    val res = reject(ellis, 20000)
    res
  }

  it should "reject C1 for mix2" in {
    val ellis = ("Ellis", "C1_mix2")
    reject(ellis, 20000)
  }
  */

  /*
  "Ellis" should "guarantee C1 for concurrent inserts" in {
    val ellis = ("Ellis", "C1_inserts")
    prove(ellis)
  }

  it should "guarantee C1 for concurrent deletes" in {
    val ellis = ("Ellis", "C1_deletes")
    prove(ellis)
  }

  it should "reject C1 for mix1" in {
    val ellis = ("Ellis", "C1_mix1")
    val res = reject(ellis, 20000)
    res
  }

  it should "reject C1 for mix2" in {
    val ellis = ("Ellis", "C1_mix2")
    reject(ellis, 20000)
  }

  it should "reject C2" in {
    val ellis = ("Ellis", "C2")
    reject(ellis)
  }

  it should "compatible commute" in {
    val ellis = ("Ellis", "compatibleCommutes")
    prove(ellis)
  }

   */
}
