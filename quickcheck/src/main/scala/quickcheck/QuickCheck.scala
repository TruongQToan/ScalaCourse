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
      x <- arbitrary[A]
      m <- oneOf(const(empty), genHeap)
    } yield insert(x, m)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val l = insert(b, h)
    def min = (a: Int, b: Int) =>
      if (a > b) b
      else a
    findMin(l) == min(a, b)
  }

  property("gen3") = forAll { a: Int =>
    val h = insert(a, empty)
    val l = deleteMin(h)
    isEmpty(l)
  }

  property("gen4") = forAll { (a: H) =>
    def check: H => Boolean = (a: H) =>
      if (isEmpty(a)) true
      else {
        val min1 = findMin(a)
        val a1 = deleteMin(a)
        if (isEmpty(a1)) true
        else if (ord.lteq(min1, findMin(a1))) check(a1)
        else false
      }
    check(a)
  }

  property("gen5") = forAll { (a: H, b: H) =>
    if (!isEmpty(a) && !isEmpty(b)) {
      val min1 = findMin(a)
      val min2 = findMin(b)
      val min3 = findMin(meld(a, b))
      min1 == min3 || min2 == min3
    } else if (isEmpty(a) && !isEmpty(b)) {
      val min = findMin(b)
      val min1 = findMin(meld(a, b))
      min == min1
    } else if (!isEmpty(a) && isEmpty(b)) {
      val min = findMin(a)
      val min1 = findMin(meld(a, b))
      min == min1
    } else true
  }

  property("gen6") = forAll { (a: H, b: H) =>
    def flatten(a: H, list: List[A]): List[A] = {
      if (isEmpty(a)) list
      else {
        val min = findMin(a)
        flatten(deleteMin(a), min :: list)
      }
    }

    val aFlat = flatten(a, List.empty)
    val bFlat = flatten(b, List.empty)
    val abFlat = flatten(meld(a, b), List.empty)
    val ab = aFlat ::: bFlat
    ab.sorted == abFlat.reverse
  }
}
