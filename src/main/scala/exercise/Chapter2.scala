package exercise

import scala.annotation.tailrec

object Chapter2 {
  // 2.1
  def fib(n: Int): Int = {
    @tailrec
    def _fib(n: Int, curr: Int, proceed: Int): Int = {
      if (n <= 0) {
        curr
      } else {
        _fib(n - 1, curr + proceed, curr)
      }
    }

    _fib(n, 0, 1)
  }

  // 2.2
  def isSored[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.sliding(2).forall(r => ordered(r.head, r.last))
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
