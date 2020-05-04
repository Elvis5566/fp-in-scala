package exercise

import org.scalatest._
import exercise.Chapter6._

class Chapter6Spec extends FunSuite {
  val seed = 1588502771000L
  val rng = SimpleRNG(seed)

  test("SimpleRNG") {
    val (n1, rng1) = rng.nextInt
    val (n2, rng2) = rng1.nextInt
    val (n3, rng3) = rng2.nextInt

    println(s"n1 + $n1")
    println(s"n2 + $n2")
    println(s"n3 + $n3")
  }

  test("6.1 nonNegativeInt") {
    val (n1, rng1) = rng.nextInt
    val (n2, rng2) = rng1.nextInt
    val (n3, rng3) = nonNegativeInt(rng2)

    println(s"n3 + $n3")
    assert(n3 > 0)
  }

  test("6.5 doubleWithMap") {
    val (n1, rng1) = double(rng)
    val (n2, rng2) = doubleWithMap(rng)
    assert(n1 == n2)
  }

  test("6.7 intsWithSequence") {
    println("intsWithSequence: " + intsWithSequence(5)(rng))
  }

  test("6.11 simulateMachine") {
    val ((coins1, candies1), _) = simulateMachine(List(Coin, Turn, Coin, Turn)).run(Machine(locked = true, 5, 10))
    assert(coins1 == 12)
    assert(candies1 == 3)

    val ((coins2, candies2), _) = simulateMachine(List(Coin, Coin)).run(Machine(locked = true, 5, 10))
    assert(coins2 == 11)
    assert(candies2 == 5)

    val ((coins3, candies3), _) = simulateMachine(List(Turn, Turn)).run(Machine(locked = true, 5, 10))
    assert(coins3 == 10)
    assert(candies3 == 5)
  }
}
