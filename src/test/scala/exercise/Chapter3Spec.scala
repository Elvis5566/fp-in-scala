package exercise

import exercise.Chapter3._
import exercise.Chapter3.List._
import org.scalatest._

class Chapter3Spec extends FunSuite {
  test("3.2 tail") {
    val l = List(1, 2, 3, 4, 5)
    assert(tail(l) == List(2, 3, 4, 5))
  }

  test("3.3 setHead") {
    val l = List(1, 2, 3, 4, 5)
    assert(setHead(l, 6) == List(6, 2, 3, 4, 5))
  }

  test("3.4 setHead") {
    val l = List(1, 2, 3, 4, 5)
    assert(drop(l, 2) == List(3, 4, 5))
  }

  test("3.5 dropWhile") {
    val l = List(1, 2, 3, 4, 5, 4, 3, 2, 1)
    assert(dropWhile(l)(_ < 4) == List(4, 5, 4, 3, 2, 1))
  }

  test("3.6 init") {
    assert(init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))
    assert(init(List(1)) == Nil)
  }

  test("foldRight") {
    assert(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3))
  }

  test("3.9 length") {
    assert(length(List(1, 2, 3, 4, 5)) == 5)
  }

  test("3.10 foldLeft") {
    assert(foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) == 15)
  }

  test("3.11 sum/product with foldLeft") {
    assert(sum(List(1, 2, 3, 4, 5)) == 15)
    assert(product(List(1, 2, 3, 4, 5)) == 120)
  }

  test("3.12 reverse") {
    assert(reverse(List(1, 2, 3, 4, 5)) == List(5, 4, 3, 2, 1))
  }

  test("3.13 foldRightWithFoldLeft") {
    assert(foldRightWithFoldLeft(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3))
  }

  test("3.14 appendWithFoldLeft/ appendWithFoldRight") {
    assert(appendWithFoldLeft(List(1, 2, 3), 4) == List(1, 2, 3, 4))
    assert(appendWithFoldRight(List(1, 2, 3), 4) == List(1, 2, 3, 4))
  }

  test("3.15 concatenateLists") {
    assert(concatenateLists(List(List(1, 2, 3), List(4, 5, 6))) == List(1, 2, 3, 4, 5, 6))
  }

  test("3.16 add1ToIntList") {
    assert(add1ToIntList(List(1, 2, 3)) == List(2, 3, 4))
  }

  test("3.17 listDoubleMapToString") {
    assert(listDoubleMapToString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))
  }

  test("3.18 map") {
    assert(map(List(1, 2, 3))(_ * 10) == List(10, 20, 30))
  }

  test("3.19 filter") {
    assert(filter(List(-1, 2, -2, 3))(_ > 0) == List(2, 3))
  }

  test("3.20 flatMap") {
    assert(flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }

  test("3.21 filterWithFlapMap") {
    assert(filterWithFlapMap(List(-1, 2, -2, 3))(_ > 0) == List(2, 3))
  }

  test("3.22 addCorrespondingElements") {
    assert(addCorrespondingElements(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
  }

  test("3.23 zipWith") {
    assert(zipWith(List(1, 2, 3), List(4, 5, 6)) { case (a, b) => a + b } == List(5, 7, 9))
  }

  test("3.24 hasSubsequence") {
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
    assert(!hasSubsequence(List(1, 2, 3, 4), List(3, 2)))
  }

  test("3.25 size") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    assert(size(tree) == 5)
  }

  test("3.26 maximum") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    assert(maximum(tree) == 3)
  }

  test("3.27 depth") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    assert(depth(tree) == 3)
  }
}
