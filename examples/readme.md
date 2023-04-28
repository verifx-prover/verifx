# Examples

Example applications of VeriFx include the verification of
Conflict-free Replicated Data Types (CRDTs) and
Operational Transformation (OT) functions described in our [paper](https://arxiv.org/abs/2207.02502).

## CRDT Examples

The source code for the implementation of the different CRDTs can be found under `CRDT Verification/src/main/verifx/org/verifx/crdtproofs`.
That directory contains several other directories for counter CRDTs, set CRDTs, etc.
The table below contains all implemented CRDTs and specifies in which directory and in which file they are defined.

| CRDT                                       | Type | File                                   |
|--------------------------------------------|------|----------------------------------------|
| Counter                                    | O    | counters/Counter.vfx                   |
| Grow-Only Counter                          | S    | counters/GCounter.vfx                  |
| Grow-Only Counter                          | D    | counters/DeltaGCounter.vfx             |
| Dynamic Grow-Only Counter                  | S    | counters/DGCounter.vfx                 |
| Positive-Negative Counter                  | S    | counters/PNCounter.vfx                 |
| Positive-Negative Counter                  | D    | counters/DeltaPNCounter.vfx            |
| Dynamic Positive-Negative Counter          | S    | counters/DPNCounter.vfx                |
| Lex Counter                                | D    | counters/LexCounter.vfx                |
| Causal Counter                             | D    | counters/CCounter.vfx                  |
| Enable-Wins Flag                           | P    | pure/flags/PureEWFlag.vfx              |
| Enable-Wins Flag                           | D    | flags/DeltaEWFlag.vfx                  |
| Enable-Wins Flag                           | O    | flags/AntidoteEWFlag.vfx               |
| Disable-Wins Flag                          | P    | pure/flags/PureDWFlag.vfx              |
| Disable-Wins Flag                          | D    | flags/DeltaDWFlag.vfx                  |
| Disable-Wins Flag                          | O    | flags/AntidoteDWFlag.vfx               |
| Multi-Value Register                       | S    | registers/MVRegister.vfx               |
| Multi-Value Register                       | D    | registers/DeltaOptimizedMVRegister.vfx |
| Multi-Value Register                       | P    | pure/registers/PureMVRegister.vfx      |
| Last-Writer-Wins Register                  | S    | registers/LWWRegister.vfx              |
| Last-Writer-Wins Register                  | O    | registers/OpBasedLWWRegister.vfx       |
| Grow-Only Set                              | O    | sets/OpBasedGSet.vfx                   |
| Grow-Only Set                              | S    | sets/GSet.vfx                          |
| Grow-Only Set                              | D    | sets/DeltaGSet.vfx                     |
| Two-Phase Set                              | O    | sets/TwoPSet.vfx                       |
| Two-Phase Set                              | S    | sets/TwoPSetSB.vfx                     |
| Two-Phase Set                              | D    | sets/DeltaTwoPSet.vfx                  |
| Unique Set                                 | O    | sets/USet.vfx                          |
| Add-Wins Set                               | P    | pure/sets/PureAWSet.vfx                |
| Remove-Wins Set                            | P    | pure/sets/PureRWSet.vfx                |
| Last-Writer-Wins Set                       | S    | sets/LWWSet.vfx                        |
| Remove-Wins Last-Writer-Wins Set           | D    | sets/RWLWWSet.vfx                      |
| Positive-Negative Set                      | S    | sets/PNSet.vfx                         |
| Observed-Removed Set                       | O    | sets/ORSet.vfx                         |
| Observed-Removed Set                       | S    | sets/ORSetSB.vfx                       |
| Optimized Observed-Removed Set             | S    | sets/OptORSet.vfx                      |
| Add-Wins Observed-Removed Set              | D    | sets/DeltaORSet.vfx                    |
| Optimized Add-Wins Observed-Removed Set    | D    | sets/DeltaOptimizedORSet.vfx           |
| Optimized Remove-Wins Observed-Removed Set | D    | sets/DeltaRWORSet.vfx                  |
| Molli, Weiss, Skaf Set                     | O    | sets/MWSSet1.vfx and sets/MWSSet2.vfx  |
| Grow-Only Map                              | S    | maps/GMap.vfx                          |
| Buggy Map                                  | O    | maps/KMap.vfx                          |
| Corrected Map                              | O    | maps/KMapFixed.vfx                     |
| 2P2P Graph                                 | O    | graphs/TwoPTwoPGraph.vfx               |
| 2P2P Graph                                 | S    | graphs/TwoPTwoPGraphSB.vfx             |
| Add-Only Directed Acyclic Graph            | O    | graphs/AddOnlyDAG.vfx                  |
| Add-Only Directed Acyclic Graph            | S    | graphs/AddOnlyDAGSB.vfx                |
| Add-Remove Partial Order                   | O    | graphs/AddRemovePartialOrder.vfx       |
| Add-Remove Partial Order                   | S    | graphs/AddRemovePartialOrderSB.vfx     |
| Replicated Growable Array (not verified)   | O    | sequences/RGA.vfx                      |
| Continuous Sequence                        | O    | sequences/ContinuumSeq.vfx             |
| Continuous Sequence                        | S    | sequences/ContinuumSeqSB.vfx           |

## OT Examples

The source code for the implementation of the different OT functions can be found under `OT Verification/src/main/verifx/org/verifx/otproofs`.
In that directory you will find several VeriFx source code files.
The table below shows in which file each of the transformation functions is implemented.

| Transformation Function | File                |
|-------------------------|---------------------|
| Ellis and Gibbs [24]    | Ellis.vfx           |
| Ressel et al. [60]      | Ressel.vfx          |
| Sun et al. [68]         | Sun.vfx             |
| Suleiman et al. [65]    | Suleiman.vfx        |
| Imine et al. [33]       | Imine.vfx           |
| Register V1 [32]        | RegisterImine.vfx   |
| Register V2 [32]        | RegisterImineV2.vfx |
| Register V3 [32]        | RegisterImineV3.vfx |
| Stack [32]              | StackImine.vfx      |
