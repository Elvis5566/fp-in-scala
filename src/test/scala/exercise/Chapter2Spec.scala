package exercise

import exercise.Chapter2._
import org.scalatest._

class Chapter2Spec extends FunSuite {
  test("2.1 fib") {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(2) == 1)
    assert(fib(3) == 2)
    assert(fib(4) == 3)
    assert(fib(5) == 5)
    assert(fib(6) == 8)
  }

  test("2.2 isSored") {
    assert(isSored[Int](Array(1, 2, 3, 4, 5), _ < _))
    assert(!isSored[Int](Array(1, 2, 3, 6, 4, 5), _ < _))
  }

}
