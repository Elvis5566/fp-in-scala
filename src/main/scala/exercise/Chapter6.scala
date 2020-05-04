package exercise

object Chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = State { s =>
      val (a1, s1) = run(s)
      (f(a1), s1)
    }

    def flatMap[B](g: A => State[S, B]): State[S, B] = State { s =>
      val (n1, s1) = run(s)
      val (n2, s2) = g(n1).run(s1)
      (n2, s2)
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State { s =>
      (a, s)
    }

    def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
      val (a, s1) = ra.run(s)
      val (b, s2) = rb.run(s1)
      (f(a, b), s2)
    }

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
      fs.foldRight(unit[S, List[A]](Nil: List[A]))(map2(_, _)(_ :: _))
    }
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng1) = rng.nextInt
    (i.abs, rng1)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    (i.abs / Int.MaxValue.toDouble, rng1)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = rng1.nextInt
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (Nil, rng)
    } else {
      val (i1, rng1) = rng.nextInt
      val (as, rng2) = ints(count - 1)(rng1)
      (i1 :: as, rng2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng =>
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  // 6.5
  def doubleWithMap: Rand[Double] = map(nonNegativeInt) { i =>
    i.abs / Int.MaxValue.toDouble
  }

  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb {
      rng1
    }
    (f(a, b), rng2)
  }

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(Nil: List[A]))(map2(_, _)(_ :: _))
  }

  def intsWithSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)((rng: RNG) => rng.nextInt))

  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (n1, rng1) = f(rng)
    val (n2, rng2) = g(n1)(rng1)
    (n2, rng2)
  }

  // 6.9
  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  // 6.11
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State.sequence(inputs.map { input =>
      State[Machine, (Int, Int)] { s =>
        (input, s) match {
          case (Coin, Machine(true, candies, coins)) if candies > 0 =>
            val newCoins = coins + 1
            ((newCoins, candies), s.copy(locked = false, coins = newCoins))
          case (Turn, Machine(false, candies, coins)) =>
            val newCandies = candies - 1
            ((coins, newCandies), s.copy(locked = true, candies = newCandies))
          case _ =>
            ((s.coins, s.candies), s)
        }
      }
    }).map(_.last)
  }
}
