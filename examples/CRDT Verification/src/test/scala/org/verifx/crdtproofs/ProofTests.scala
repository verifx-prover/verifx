package org.verifx.crdtproofs

import org.scalatest.FlatSpec

class ProofTests extends FlatSpec with Prover {
  "OpBasedGSet" should "be a CmRDT" in {
    val set = ("OpBasedGSet", "is_a_CmRDT")
    prove(set)
  }

  "DeltaGSet" should "be a CvRDT" in {
    val set = ("DeltaGSet", "is_a_CvRDT")
    prove(set)
  }

  "DeltaTwoPSet" should "be a CvRDT" in {
    val set = ("DeltaTwoPSet", "is_a_CvRDT")
    prove(set)
  }

  "Delta GCounter" should "be a CvRDT" in {
    val ctr = ("DeltaGCounter", "is_a_CvRDT")
    prove(ctr)
  }

  "DGCounter" should "be a CvRDT" in {
    val counter = ("DGCounter", "is_a_CvRDT")
    prove(counter)
  }

  "Delta PNCounter" should "be a CvRDT" in {
    val ctr = ("DeltaPNCounter", "is_a_CvRDT")
    prove(ctr)
  }

  "LexCounter" should "be a CvRDT" in {
    val ctr = ("LexCounter", "is_a_CvRDT")
    prove(ctr)
  }

  "CCounter" should "be a CvRDT" in {
    val ctr = ("CCounter", "is_a_CvRDT")
    prove(ctr)
  }

  "Delta ORSet" should "be a CvRDT" in {
    val set = ("DeltaORSet", "is_a_CvRDT")
    prove(set)
  }

  "Delta Optimized ORSet" should "be a CvRDT" in {
    val set = ("DeltaOptimizedORSet", "is_a_CvRDT")
    prove(set)
  }

  "DeltaRWORSet" should "be a CvRDT" in {
    val set = ("DeltaRWORSet", "is_a_CvRDT")
    prove(set)
  }

  "RWLWWSet" should "be a CvRDT" in {
    val set = ("RWLWWSet", "is_a_CvRDT")
    prove(set)
  }

  "Delta MVReg" should "be a CvRDT" in {
    val reg = ("DeltaOptimizedMVRegister", "is_a_CvRDT")
    prove(reg)
  }

  "DeltaDWFlag" should "be a CvRDT" in {
    val flag = ("DeltaDWFlag", "is_a_CvRDT")
    prove(flag)
  }

  "DeltaEWFlag" should "be a CvRDT" in {
    val flag = ("DeltaEWFlag", "is_a_CvRDT")
    prove(flag)
  }

  "AntidoteEWFlag" should "a CmRDT" in {
    val flag = ("AntidoteEWFlag", "is_a_CmRDT")
    prove(flag)
  }

  "AntidoteDWFlag" should "a CmRDT" in {
    val flag = ("AntidoteDWFlag", "is_a_CmRDT")
    prove(flag)
  }

  "KMap" should "not putDelCommute" in {
    val set = ("KMap", "putDelCommute")
    rejectForModel(set)
  }

  it should "not delPutCommute" in {
    val set = ("KMap", "delPutCommute")
    rejectForModel(set)
  }

  "KMapFixed" should "putPutCommute" in {
    val set = ("KMapFixed", "putPutCommute")
    prove(set)
  }

  it should "putDelCommute" in {
    val set = ("KMapFixed", "putDelCommute")
    prove(set)
  }

  it should "delPutCommute" in {
    val set = ("KMapFixed", "delPutCommute")
    prove(set)
  }

  it should "delDelCommute" in {
    val set = ("KMapFixed", "delDelCommute")
    prove(set)
  }

  //////////////////////////////////
  // Operation-based Counter CRDT //
  //////////////////////////////////

  "Counter" should "be a CmRDT" in {
    val ctr = ("Counter", "is_a_CmRDT")
    val res = prove(ctr)
    res
  }

  ////////////////////////////////////////
  // State-based Grow-only Counter CRDT //
  ////////////////////////////////////////

  "GCounter" should "be a CvRDT" in {
    val proof = ("GCounter", "is_a_CvRDT")
    prove(proof)
  }

  ////////////////////////////////////////////////
  // State-based Positive-Negative Counter CRDT //
  ////////////////////////////////////////////////

  "PNCounter" should "be a CvRDT" in {
    val proof = ("PNCounter", "is_a_CvRDT")
    prove(proof)
  }

  "PNCounter2" should "be a CvRDT" in {
    val pnCounter = ("PNCounter2", "is_a_CvRDT")
    prove(pnCounter)
  }

  ////////////////////////////////////////////////////////
  // State-based Dynamic Positive-Negative Counter CRDT //
  ////////////////////////////////////////////////////////

  "DPNCounter" should "be a CvRDT" in {
    val counter = ("DPNCounter", "is_a_CvRDT")
    prove(counter)
  }

  ///////////////////////////////////////////
  // State-based Multi-Value Register CRDT //
  ///////////////////////////////////////////

  "MVRegister" should "be a CvRDT" in {
    val counter = ("MVRegister", "is_a_CvRDT")
    prove(counter)
  }

  ////////////////////////////////////////////////
  // State-based Last-Writer-Wins Register CRDT //
  ////////////////////////////////////////////////

  "LWWRegister" should "be a CvRDT" in {
    val lwwReg = ("LWWRegister", "is_a_CvRDT")
    prove(lwwReg)
  }

  /////////////////////////////////////////////
  // Op-based Last-Writer-Wins Register CRDT //
  /////////////////////////////////////////////

  it should "be a CmRDT" in {
    val lwwReg = ("OpBasedLWWRegister", "is_a_CmRDT")
    prove(lwwReg)
  }

  ////////////////////////////////////
  // State-based Grow-Only Set CRDT //
  ////////////////////////////////////

  "GSet" should "be a CvRDT" in {
    val gset = ("GSet", "is_a_CvRDT")
    prove(gset)
  }

  ////////////////////////////////////
  // State-based Two-Phase Set CRDT //
  ////////////////////////////////////

  "2PSet" should "be a CvRDT" in {
    val set = ("TwoPSetSB", "is_a_CvRDT")
    prove(set)
  }

  /////////////////////////////////
  // Op-based Two-Phase Set CRDT //
  /////////////////////////////////

  it should "be a CmRDT" in {
    val twoPSet = ("TwoPSet", "is_a_CmRDT")
    prove(twoPSet)
  }

  //////////////////////////////
  // Op-based Unique Set CRDT //
  //////////////////////////////

  "USet" should "be a CmRDT" in {
    val uset = ("USet", "is_a_CmRDT")
    prove(uset)
  }

  ///////////////////////////////////////////
  // State-based Last-Writer-Wins Set CRDT //
  ///////////////////////////////////////////

  "LWWSet" should "be a CvRDT" in {
    val lwwSet = ("LWWSet", "is_a_CvRDT")
    prove(lwwSet)
  }

  // Optimized state-based Last-Writer-Wins Set
  "EfficientLWWSet" should "be commutative" in {
    val set = ("EfficientLWWSet", "mergeCommutative")
    prove(set, 5, 20)
  }

  it should "be idempotent" in {
    val set = ("EfficientLWWSet", "mergeIdempotent")
    prove(set, 5, 20)
  }

  it should "be associative" in {
    val set = ("EfficientLWWSet", "mergeAssociative")
    prove(set, 5, 20)
  }

  ////////////////////////////////////////////
  // State-based Positive-Negative Set CRDT //
  ////////////////////////////////////////////

  "PNSet" should "be a CvRDT" in {
    val pnSet = ("PNSet", "is_a_CvRDT")
    prove(pnSet)
  }

  ///////////////////////////////////////////
  // State-based Observed-Removed Set CRDT //
  ///////////////////////////////////////////

  "Naive ORSet" should "be a CvRDT" in {
    val set = ("ORSet", "is_a_CvRDT")
    prove(set)
  }

  "ORSet" should "be a CvRDT" in {
    val set = ("ORSet2", "is_a_CvRDT")
    prove(set)
  }

  ///////////////////////////////////////////////
  // Operation-based Observed-Removed Set CRDT //
  ///////////////////////////////////////////////

  it should "be a CmRDT" in {
    val orSet = ("OpBasedORSet", "is_a_CmRDT")
    prove(orSet)
  }

  /////////////////////////////////////////////////////
  // State-based Optimized Observed-Removed Set CRDT //
  /////////////////////////////////////////////////////

  "OptORSet's merge function" should "be commutative" in {
    val set = ("OptORSet", "mergeCommutative")
    prove(set)
  }

  it should "be idempotent" in {
    val set = ("OptORSet", "mergeIdempotent")
    prove(set)
  }

  it should "be associative" in {
    val set = ("OptORSet", "mergeAssociative")
    prove(set)
  }

  ///////////////////////////////////////////////
  // Operation-based Molli-Weiss-Skaf Set CRDT //
  ///////////////////////////////////////////////

  "MWSSet1" should "be a CmRDT" in {
    val mwsSet = ("MWSSet1", "is_a_CmRDT")
    prove(mwsSet)
  }

  "MWSSet2" should "not be a CmRDT" in {
    val mwsSet = ("MWSSet2", "is_a_CmRDT")
    reject(mwsSet)
  }

  ////////////////////////////////////
  // State-based Grow-Only Map CRDT //
  ////////////////////////////////////

  "GMap" should "be a CvRDT" in {
    val gmap = ("GMap", "is_a_CvRDT")
    prove(gmap)
  }

  ///////////////////////////////////////////
  // State-based Add-Wins Nested Map CRDT //
  /////////////////////////////////////////

  "NestedMap" should "be a CvRDT" in {
    val map = ("NestedMap", "is_a_CvRDT")
    prove(map)
  }

  //////////////////////////////////////////////////////
  // State-based Add-Only Directed Acyclic Graph CRDT //
  //////////////////////////////////////////////////////

  "AddOnlyDAGSB" should "be a CvRDT" in {
    val dag = ("AddOnlyDAGSB", "is_a_CvRDT")
    prove(dag)
  }

  //////////////////////////////////////////////////////////
  // Operation-based Add-Only Directed Acyclic Graph CRDT //
  //////////////////////////////////////////////////////////

  "AddOnlyDAG" should "be a CmRDT" in {
    val dag = ("AddOnlyDAG", "is_a_CmRDT")
    prove(dag)
  }

  ///////////////////////////////////////////////
  // State-based Add-Remove Partial Order CRDT //
  ///////////////////////////////////////////////

  "AddRemovePartialOrderSB" should "be a CvRDT" in {
    val po = ("AddRemovePartialOrderSB", "is_a_CvRDT")
    prove(po)
  }

  ///////////////////////////////////////////////////
  // Operation-based Add-Remove Partial Order CRDT //
  ///////////////////////////////////////////////////

  "AddRemovePartialOrder" should "be a CmRDT" in {
    val po = ("AddRemovePartialOrder", "is_a_CmRDT")
    prove(po)
  }

  ////////////////////////////////////////////////
  // State-based Two-Phase Two-Phase Graph CRDT //
  ////////////////////////////////////////////////

  "2P2PGraphSB" should "be a CvRDT" in {
    val graph = ("TwoPTwoPGraphSB", "is_a_CvRDT")
    prove(graph)
  }

  ////////////////////////////////////////////////////
  // Operation-based Two-Phase Two-Phase Graph CRDT //
  ////////////////////////////////////////////////////

  "2P2PGraph" should "be a CmRDT" in {
    val graph = ("TwoPTwoPGraph", "is_a_CmRDT")
    prove(graph)
  }

  //////////////////////////////////////////
  // State-based Continuous Sequence CRDT //
  //////////////////////////////////////////

  "ContinuumSeq" should "be a CvRDT" in {
    val seq = ("ContinuumSeqSB", "is_a_CvRDT")
    prove(seq)
  }

  //////////////////////////////////////////////
  // Operation-based Continuous Sequence CRDT //
  //////////////////////////////////////////////

  it should "be a CmRDT" in {
    val seq = ("ContinuumSeq", "is_a_CmRDT")
    prove(seq)
  }

  /////////////////////////////////////////
  // Pure Op-based Enable-Wins Flag CRDT //
  /////////////////////////////////////////

  "Pure EWFlag" should "converge" in {
    val flag = ("PureEWFlag", "is_a_CmRDT")
    prove(flag)
  }

  //////////////////////////////////////////
  // Pure Op-based Disable-Wins Flag CRDT //
  //////////////////////////////////////////

  "Pure DWFlag" should "converge" in {
    val flag = ("PureDWFlag", "is_a_CmRDT")
    prove(flag)
  }

  ////////////////////////////////////////
  // Pure op-based Multi-Value Register //
  ////////////////////////////////////////

  "PureMVRegister" should "converge" in {
    val mvReg = ("PureMVRegister", "is_a_CmRDT")
    prove(mvReg)
  }

  /////////////////////////////////////
  // Pure Op-based Add-Wins Set CRDT //
  /////////////////////////////////////

  "PureAWSet" should "converge" in {
    val awSet = ("PureAWSet", "is_a_CmRDT")
    prove(awSet)
  }

  ////////////////////////////////////////
  // Pure Op-based Remove-Wins Set CRDT //
  ////////////////////////////////////////

  "PureRWSet" should "converge" in {
    val awSet = ("PureRWSet", "is_a_CmRDT")
    prove(awSet)
  }
}
