package exercise

import exercise.Chapter6.{RNG, State, double, nonNegativeInt}

class Chapter8 {

  case class SGen[+A](forSize: Int => Gen[A]) {
    // 8.12
    def listOf[B](g: Gen[B]): SGen[List[B]] = SGen { n =>
      g.listOfN(unit(n))
    }
  }

  case class Gen[A](sample: State[RNG, A]) {
    // 8.6
    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      //      Gen(sample.map(f)) // should be correct, event simpler?
      Gen(sample.flatMap(a => f(a).sample))
    }

    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => sequence(List.fill(n)(this)))

    // 8.10
    def unsized: SGen[A] = SGen(_ => this)
  }

  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(nonNegativeInt).map(i => start + i % (stopExclusive - start)))

  // 8.5
  //  def unit[A](a: => A): Gen[A] = Gen(State(rng => (a, rng)))
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(nonNegativeInt).map(_ % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = sequence(List.fill(n)(g))

  def sequence[A](l: List[Gen[A]]): Gen[List[A]] = Gen(State.sequence(l.map(_.sample)))

  // 8.7 XX
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  // 8.8 XX
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  object Prop {
    /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
    }

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = {
      val a = g(_)

      forAll(g(_))(f)
    }

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max,n,rng) =>
        val casesPerSize = (n - 1) / max + 1
        val props: Stream[Prop] =
          Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop =
          props.map(p => Prop { (max, n, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)
        prop.run(max,n,rng)
    }
  }
  case class Prop(run: (TestCases, RNG) => Result) {
    // 8.9
    def &&(p: Prop): Prop = Prop { (testCases, rng) =>
      run(testCases, rng) match {
        case Passed =>
          p.run(testCases, rng)
        case f@Falsified(failure, successes) =>
          f
      }
    }

    def ||(p: Prop): Prop = Prop { (testCases, rng) =>
      run(testCases, rng) match {
        case Passed =>
          Passed

        case Falsified(failure, successes) =>
          p.run(testCases, rng)
      }
    }
  }

}
