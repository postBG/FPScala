package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    h <- oneOf(Gen.const(empty), genHeap)
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("hint1") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val min = if (a < b) a else b
    findMin(h) == min
  }

  property("hint2") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("hint3") = forAll { (xs: List[Int]) =>
    def pushAll(xs: List[Int], h: H): H = xs match {
      case Nil => h
      case head::tail => pushAll(tail, insert(head, h))
    }

    def findAll(h: H, acc: List[Int]): List[Int] = {
      if (isEmpty(h)) acc
      else {
        val x = findMin(h)
        findAll(deleteMin(h), x::acc)
      }
    }

    val h = pushAll(xs, empty)
    findAll(h, List()) == xs.sorted.reverse
  }

  property("hint4") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val min = if (min1 < min2) min1 else min2

    findMin(meld(h1, h2)) == min
  }
}
