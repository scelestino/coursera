package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =  for {
    i <- arbitrary[A]
    h <- oneOf[H](empty, genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("2 elem heap, min should be min of them") = forAll { (a:Int, b:Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  property("1 elem heap, delete min should be empty heap") = forAll { a:Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("find and delete min should be sorted") = forAll { h:H =>
    toList(h) == toList(h).sorted
  }

  property("min of meld should be min of both") = forAll { (h1:H, h2:H) =>
    val h3 = meld(h1, h2)
    findMin(h3) == Math.min(findMin(h1), findMin(h2))
  }

  property("meld associativity") = forAll { (h1:H, h2:H, h3:H) =>
    toList(meld(meld(h1, h2), h3)) == toList(meld(h1, meld(h2, h3)))
  }

  private def toList(h:H):List[A] = if(isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))

}
