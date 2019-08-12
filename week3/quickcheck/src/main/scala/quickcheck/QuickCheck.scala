package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(a, h))


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("sorted") = forAll { h: H =>
    val testheap = meld(insert(3, empty), insert(1, insert(2, empty)))
    val first = findMin(testheap)
    val second = findMin(deleteMin(testheap))
    val third = findMin(deleteMin(deleteMin(testheap)))
    first == 1 && second == 2 && third == 3
  }

}
