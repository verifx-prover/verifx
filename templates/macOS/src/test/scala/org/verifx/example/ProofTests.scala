package org.verifx.example

import org.verifx.example.Prover
import org.scalatest.FlatSpec

class ProofTests extends FlatSpec with Prover {
  "Integer addition" should "commute" in {
    // the tuple below uniquely identifies the proof we want to verify
    // the first element is the name of the object containing the proof
    // the second element is the name of the proof
    val proof = ("ProofExamples", "addCommutes")
    prove(proof)
  }

  "Any two values" should "not all be equal" in {
    import scala.meta.contrib._

    val proof = ("ProofExamples", "anyValuesAreEqual")
    val proofRes = rejectForModel(proof)
    println("Counter example for 'anyValuesAreEqual' proof:")
    println(proofRes) // prints a textual representation of the counter example to the console

    proofRes.counterExample() match {
      case (List(var1, var2), _) => {
        assert(var1.name == "a" || var1.name == "b")
        assert(var2.name == "a" || var2.name == "b")
        assert(var1.tpe.isEqual(var2.tpe))
        assert(var1.name !== var2.name)
        assert(var1.value !== var2.value)
      }
      case _ => assert(false)
    }
  }
}
