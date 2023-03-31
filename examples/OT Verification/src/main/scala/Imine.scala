
sealed trait Op
case class Insert(p: Int, ip: Int, c: Int) extends Op
case class Delete(p: Int) extends Op
case class Identity() extends Op

/*
 * Implementation of the transformation functions proposed in
 * "Proving Correctness of Transformation Functions in Real-Time Groupware"
 * by Abdessamad Imine, Pascal Molli, Gérald Oster and Michaël Rusinowitch
 */
object Imine {
  def Tii(x: Insert, y: Insert): Op = {
    val p1 = x.p; val ip1 = x.ip; val c1 = x.c
    val p2 = y.p; val ip2 = y.ip; val c2 = y.c
    if (p1 < p2)
      x
    else if (p1 > p2)
      new Insert(p1 + 1, ip1, c1)
    else {
      // p1 == p2
      if (ip1 < ip2)
        x
      else if (ip1 > ip2)
        new Insert(p1 + 1, ip1, c1)
      else {
        // ip1 == ip2 --> tie break on the value of the character
        if (c1 < c2)
          x
        else if (c1 > c2)
          new Insert(p1 + 1, ip1, c1)
        else {
          // c1 == c2
          new Identity()
        }
      }
    }
  }

  def Tid(x: Insert, y: Delete): Op = {
    if (x.p > y.p)
      new Insert(x.p - 1, x.ip, x.c)
    else
      x
  }

  def Tdd(x: Delete, y: Delete): Op = {
    if (x.p < y.p)
      x
    else if (x.p > y.p)
      new Delete(x.p - 1)
    else
      new Identity()
  }

  def Tdi(x: Delete, y: Insert): Op = {
    if (x.p < y.p)
      x
    else
      new Delete(x.p + 1)
  }

  // Transforms incoming operation `x`
  // according to existing operation `y`
  def transform(x: Op, y: Op): Op = x match {
    case i1: Insert => y match {
      case i2: Insert => this.Tii(i1, i2)
      case d2: Delete => this.Tid(i1, d2)
      case _ => x // should never happen
    }
    case d1: Delete => y match {
      case i2: Insert => this.Tdi(d1, i2)
      case d2: Delete => this.Tdd(d1, d2)
      case _ => x // should never happen
    }
    case _ => x // should never happen
  }

  // The below methods are identical to those in Ellis, Ressel, and Sun
  // EXCEPT `enabled` which does an additional check that `op.p == op.ip`
  /*
  def apply(lst: List[Int], op: Op): List[Int] = op match {
    case Insert(p, _, c) => lst.insert(p, c)
    case Delete(p) => lst.delete(p)
    case _: Identity => lst
  }
  */

  // Checks if operation `op` can be executed on `state`
  def enabled(op: Op, lst: List[Int]): Boolean = op match {
    case Insert(p, ip, c) =>
      p >= 0 && p <= lst.length && p == ip // `p == ip` must always be true at the originating replica
    case Delete(p) =>
      p >= 0 && p < lst.length
    case _: Identity =>
      false // identity operations should not be generated
  }

  /*
  // Condition C1 for transformation functions expresses "semantic equivalence between two sequences".
  // See: "Proving Correctness of Transformation Functions in Real-Time Groupware"
  // by Abdessamad Imine, Pascal Molli, Gérald Oster and Michaël Rusinowitch
  def C1(): Boolean = {
    forall (opI: Op, opJ: Op, st: List[Int]) {
      (this.enabled(opI, st) && this.enabled(opJ, st)) =>:
        (this.apply(this.apply(st, opI), this.transform(opJ, opI)) == this.apply(this.apply(st, opJ), this.transform(opI, opJ)))
    }
  }
  /*def C1(opI: Op, opJ: Op, st: LList[Int]): Boolean = {
    (this.enabled(opI, st) && this.enabled(opJ, st)) =>:
    (this.apply(this.apply(st, opI), this.transform(opJ, opI)) == this.apply(this.apply(st, opJ), this.transform(opI, opJ)))
  }*/

  // Condition C2 for transformation functions expresses "syntactic equivalence between two sequences".
  // See: "Proving Correctness of Transformation Functions in Real-Time Groupware"
  // by Abdessamad Imine, Pascal Molli, Gérald Oster and Michaël Rusinowitch
  def C2(): Boolean = {
    forall (opI: Op, opJ: Op, opK: Op, st: List[Int]) {
      (this.enabled(opI, st) && this.enabled(opJ, st) && this.enabled(opK, st)) =>:
        (this.transform(this.transform(opK, opI), this.transform(opJ, opI)) == this.transform(this.transform(opK, opJ), this.transform(opI, opJ)))
    }
  }
   */

  def C2(opI: Op, opJ: Op, opK: Op, st: List[Int]): (Boolean, Op, Op, Boolean) = {
    (enabled(opI, st) && enabled(opJ, st) && enabled(opK, st),
      transform(transform(opK, opI), transform(opJ, opI)),
      transform(transform(opK, opJ), transform(opI, opJ)),
      transform(transform(opK, opI), transform(opJ, opI)) == transform(transform(opK, opJ), transform(opI, opJ)))
  }
}

/*
class Test extends App {
  val x = 120
  val y = 121

  val opI = Insert(2, 2, x)
  val opJ = Delete(2)
  val opK = Insert(3, 3, y)

  val st = List(97, 98, 99)

  val res = Imine.C2(opI, opJ, opK, st)
  res

  /////

  val opI = Insert(2, 2, x)
  val opJ = Delete(1)
  val opK = Insert(2, 2, y)

  val st = List(97, 98, 99, 100)

  val res = Imine.C2(opI, opJ, opK, st)
  res
}
*/
