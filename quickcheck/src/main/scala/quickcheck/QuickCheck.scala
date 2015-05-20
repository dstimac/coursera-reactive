package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // Generator
  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e, h)


////  If you insert any two elements into an empty heap, finding the
////  minimum of the resulting heap should get the smallest of the two elements back.
//  property("minOfTwo") = forAll { (a: Int, b: Int) =>
//    val h1 = insert(b, insert(a, empty))
//    val h2 = insert(a, insert(b, empty))
//    a < b match {
//      case true =>
//        findMin(h1) == a & findMin(h2) == a
//      case _ =>
//        findMin(h1) == b & findMin(h2) == b
//    }
//  }

  //  If you insert any two elements into an empty heap, finding the
  //  minimum of the resulting heap should get the smallest of the two elements back.
  property("minOfTwo") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (a < b match {
      case true => a
      case _ => b
    })
  }

//  If you insert an element into an empty heap, then delete the
//  minimum, the resulting heap should be empty.
  property("removeOne") = forAll { a: Int =>
    deleteMin(insert(a, empty)) == empty
  }

//  Given any heap, you should get a sorted sequence of elements
//  when continually finding and deleting minima. (Hint: recursion and
//  helper functions are your friends.)
  property("orderedHeap") = forAll{ h: H =>
    def loop(h: H, prev: Int): Boolean = {
      val reduced = deleteMin(h)
      prev <= findMin(h) && (isEmpty(reduced) || loop(reduced, findMin(h)))
    }
    h == empty || loop(h, Int.MinValue)
  }

//  Finding a minimum of the melding of any two heaps should return a
//  minimum of one or the other.
  property("minOfMeld") = forAll { (h1: H, h2: H)=>
    val min = findMin(meld(h1, h2))
    min == findMin(h1) || min == findMin(h2)
  }

//  Given any (ordered) list, check findMin(h) == list.head
  property("orderedList") = forAll{ list: List[Int] =>
    def fill(current: H, tail: List[Int]): H = {
      tail.isEmpty match {
        case true =>
          current
        case _ =>
          fill(insert(tail.head, current), tail.tail)
      }
    }
    def checkOrdering(list: List[Int], h: H): Boolean = {
      list.isEmpty match {
        case true => isEmpty(h)
        case _ =>
          !isEmpty(h) && findMin(h) == list.head && checkOrdering(list.tail, deleteMin(h))
      }
    }
    val h: H = fill(empty, list)
    checkOrdering(list.sorted, h)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
