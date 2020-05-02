package exercise

import exercise.Chapter5._
import org.scalatest._

class Chapter5Spec extends FunSuite {
  test("5.1 toList") {
    assert(Stream(1, 2, 3, 4, 5).toList == List(1, 2, 3, 4, 5))
  }

  test("5.2 take/drop") {
    assert(Stream(1, 2, 3, 4, 5).take(1).toList == List(1))
    assert(Stream(1, 2, 3, 4, 5).take(2).toList == List(1, 2))
    assert(Stream(1, 2, 3, 4, 5).drop(2).toList == List(3, 4, 5))
  }

  test("5.3 takeWhile") {
    assert(Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList == List(1, 2, 3))
  }

  test("5.4 forAll") {
    assert(Stream(1, 2, 3, 4, 5).forAll(_ > 0))
    assert(!Stream(1, 2, 3, 4, 5).forAll(_ > 1))
  }

  test("5.5 takeWhileWithFoldRight") {
    assert(Stream(1, 2, 3, 4, 5).takeWhileWithFoldRight(_ < 4).toList == List(1, 2, 3))
  }

  test("5.6 headOption") {
    assert(Stream(1, 2, 3, 4, 5).headOption.contains(1))
    assert(Empty.headOption.isEmpty)
  }

  test("5.7 map/filter") {
    assert(Stream(1, 2, 3, 4, 5).map(_ + 1).toList == List(2, 3, 4, 5, 6))
    Stream(1, 2, 3, 4, 5).filter(_ < 3)
  }


  test("5.8 constant") {
    assert(Stream.constant(2).take(5).toList == List(2, 2, 2, 2, 2))
  }

  test("5.9 from") {
    assert(Stream.from(10).take(5).toList == List(10, 11, 12, 13, 14))
  }

  test("5.10 fibs") {
    assert(Stream.fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  test("5.11 unfold") {
    assert(Stream.unfold(1)(s => if (s < 5) Some((s, s + 1)) else None).toList == List(1, 2, 3, 4))
  }

  test("5.12 fibsWithUnfold, fromWithUnfold, constantWithUnfold, onesWithUnfold") {
    assert(Stream.fibsWithUnfold.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
    assert(Stream.fromWithUnfold(10).take(5).toList == List(10, 11, 12, 13, 14))
    assert(Stream.constantWithUnfold(2).take(5).toList == List(2, 2, 2, 2, 2))
    assert(Stream.onesWithUnfold.take(5).toList == List(1, 1, 1, 1, 1))
  }

  test("5.13 mapWithUnfold, takeWithUnfold, takeWhileWithUnfold, zipWith") {
    assert(Stream(1, 2, 3, 4, 5).mapWithUnfold(_ + 1).toList == List(2, 3, 4, 5, 6))
    assert(Stream(1, 2, 3, 4, 5).takeWithUnfold(2).toList == List(1, 2))
    assert(Stream(1, 2, 3, 4, 5).takeWhileWithUnfold(_ < 4).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3, 4, 5).zipWith(Stream(6, 7, 8, 9)).toList == List((1, 6), (2, 7), (3, 8), (4, 9)))
    assert(Stream(1, 2, 3, 4).zipAll(Stream(5, 6, 7)).toList == List((Some(1), Some(5)), (Some(2), Some(6)), (Some(3), Some(7)), (Some(4), None)))
  }

  test("5.14 startsWith") {
    assert(Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2)))
    assert(Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3)))
    assert(!Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 4)))
  }

  test("5.15 tails") {
    assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
  }

  test("scanRight") {
    assert(Stream(1, 2, 3).tailsWithScanRight.toList.map(_.toList) == List(List(1, 2, 3), List(2, 3), List(3), Nil))
  }

  test("tailsWithScanRight") {
    assert(Stream(1, 2, 3).tailsWithScanRight.toList.map(_.toList) == List(List(1, 2, 3), List(2, 3), List(3), Nil))
  }

}
