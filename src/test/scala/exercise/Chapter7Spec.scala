package exercise

import org.scalatest._
import Chapter7.Par._
import java.util.concurrent.Executors

class Chapter7Spec extends FunSuite {
  val es = Executors.newFixedThreadPool(10)

  test("7.4 asyncF") {
    def intToString(n: Int) = n.toString
    val par = asyncF(intToString)
    assert(par(2)(es).get() == "2")
    println("thread: " + Thread.currentThread())
  }

  test("map2") {
    val a = lazyUnit {
      println("a thread: " + Thread.currentThread())
      1
    }

    val b = lazyUnit {
      println("b thread: " + Thread.currentThread())
      2
    }

    val r1 = map2(a, b)(_ + _)(es)
    val r2 = fork(map2(a, b)(_ + _))(es)
//    assert(r.get == 3)
  }
}
