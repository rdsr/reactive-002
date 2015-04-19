package quickcheck


import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min of 2") = forAll {
    (a: Int, b: Int) =>
      val h = insert(a, (insert(b, empty)))
      findMin(h) == Math.min(a, b)
  }

  property("Add to empty then delete") = forAll {
    a: Int =>
      isEmpty(deleteMin(insert(a, empty)))
  }

  property("First element is the minimum in the heap") = forAll {
    h: H => isEmpty(h) ||
      isEmpty(deleteMin(h)) || // single element heap
      ord.compare(findMin(h), findMin(deleteMin(h))) <= 0 // more than 1 element in Heap
  }

  property("min of two heaps") = forAll {
    (h1: H, h2: H) =>
      val h = meld(h1, h2)
      if(isEmpty(h1) && isEmpty(h2)) {
        isEmpty(h)
      } else if (isEmpty(h1)) {
        findMin(h) == findMin(h2)
      } else if (isEmpty(h2)) {
        findMin(h) == findMin(h1)
      } else {
        findMin(h) == Math.min(findMin(h1), findMin(h2))
      }
  }

  def size(h: H): Int = {
    if (isEmpty(h)) 0
    else 1 + size(deleteMin(h))
  }

  property("Size of combined heap") = forAll {
    (h1: H, h2: H) => size(h1) + size(h2) == size(meld(h1, h2))
  }

  property("Size of heap after delete") = forAll {
    (h: H) => isEmpty(h) || size(deleteMin(h)) == size(h) - 1
  }

  def toHeap(l: List[Int]): H = l match {
    case Nil => empty
    case h :: t => insert(h, toHeap(t))
  }

  def toList(h: H): List[Int] = {
    if (isEmpty(h)) Nil
    else
      findMin(h) :: toList(deleteMin(h))
  }

  property("What goes in should come out") = forAll {
    l: List[Int] =>
      val h = toHeap(l)
      val sortedL = toList(h)
      sortedL == l.sorted
  }

  lazy val genNode = for {
    v <- arbitrary[Int]
    sH <- genHeap
  } yield insert(v, sH)

  lazy val genHeap: Gen[H] = oneOf(const(empty), genNode)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
