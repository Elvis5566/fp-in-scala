package exercise

import org.scalatest._

class Chapter4Spec extends Suites(
  new OptionSpec(),
  new EitherSpec()
)

private class OptionSpec extends FunSuite {

  import exercise.Chapter4.Option._

  test("4.1 Option") {
    assert(Some(1).map(_ + 1) == Some(2))
    assert(None.map(a => a) == None)
    assert(Some(1).flatMap(_ => Some(2)) == Some(2))
    assert(None.flatMap(a => a) == None)
    assert(Some(1).getOrElse(2) == 1)
    assert(None.getOrElse(2) == 2)
    assert(Some(1).orElse(Some(2)) == Some(1))
    assert(None.orElse(Some(2)) == Some(2))
    assert(Some(1).filter(_ > 0) == Some(1))
    assert(Some(1).filter(_ > 1) == None)
    assert(None.filter(_ => true) == None)
  }

  test("4.3 map2") {
    assert(map2(Some(1), Some(2))(_ + _) == Some(3))
    assert(map2(Some(1), None)(_ + _) == None)
  }

  test("4.4 sequence") {
    assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
    assert(sequence(List(Some(1), None, Some(3))) == None)
  }

  test("4.4 traverse") {
    assert(traverse(List(1, 2, 3))(Some(_)) == Some(List(1, 2, 3)))
    assert(traverse(List(1, 2, 3))(i => if (i < 2) Some(i) else None) == None)
  }
}

private class EitherSpec extends FunSuite {

  import exercise.Chapter4.Either._

  test("4.6 Either") {
    assert(Right(1).map(_ + 1) == Right(2))
    assert(Left(1).map(a => a) == Left(1))
    assert(Right(1).flatMap(_ => Right(2)) == Right(2))
    assert(Left(1).flatMap(_ => Right(2)) == Left(1))
    assert(Right(1).orElse(Right(2)) == Right(1))
    assert(Left(1).orElse(Right(2)) == Right(2))
    assert(Right(1).map2(Right(2))(_ + _) == Right(3))
  }

  test("4.7 sequence") {
    assert(sequence(List(Right(1), Right(2), Right(3))) == Right(List(1, 2, 3)))
    assert(sequence(List(Right(1), Left(2), Right(3))) == Left(2))
    assert(traverse(List(1, 2, 3))(Right(_)) == Right(List(1, 2, 3)))
    assert(traverse(List(1, 2, 3))(i => if (i == 2) Left(i) else Right(i)) == Left(2))
  }


}