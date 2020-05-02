package exercise

import scala.annotation.tailrec

object Chapter5 {

  import Stream._

  sealed trait Stream[+A] {
    // 5.1
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, tl) => h() :: tl().toList
    }

    // 5.2
    def take(n: Int): Stream[A] = {
      this match {
        case Cons(h, tl) if n > 0 => cons(h(), tl().take(n - 1))
        case _ => Empty
      }
    }

    def drop(n: Int): Stream[A] = {
      this match {
        case Cons(_, tl) => if (n == 0) this else tl().drop(n - 1)
        case _ => Empty
      }
    }

    // 5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, tl) if p(h()) => cons(h(), tl().takeWhile(p))
      case _ => Empty
    }

    // 5.4
    def forAll(p: A => Boolean): Boolean = this match {
      case Empty => true
      case Cons(h, tl) => if (p(h())) tl().forAll(p) else false
    }

    // f has by name argument, so this foldRight can early exit, but it seems semantically wrong.
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    // 5.5
    def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
      foldRight(empty[A]) { (a, b) =>
        if (p(a)) {
          println("takeWhileWithFoldRight check1")
          cons(a, b)
        } else {
          println("takeWhileWithFoldRight check2")
          Empty
        }
      }

    // 5.6
    def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

    // 5.7
    def map[B](f: A => B): Stream[B] = this match {
      case Empty => Empty
      case Cons(h, tl) => cons(f(h()), tl().map(f))
    }

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A]) { (a, b) =>
        if (p(a)) {
          cons(a, b)
        } else {
          // filter will filter out all element immediately if p(a) == false, it should be evaluated only when needed.
          b
        }
      }

    def append[B >: A](a: => Stream[B]): Stream[B] =
      foldRight(a)((h, acc) => cons(h, acc))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B]) { (h, t) =>
        f(h).append(t)
      }

    // 5.13
    def mapWithUnfold[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h, tl) => Some((f(h()), tl()))
      case _ => None
    }

    def takeWithUnfold(n: Int): Stream[A] = unfold((n, this)) {
      case (0, _) => None
      case (n, Cons(h, tl)) => Some((h(), (n - 1, tl())))
    }

    def takeWhileWithUnfold(p: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, tl) if p(h()) => Some((h(), tl()))
      case _ => None
    }

    def zipWith[B](bs: Stream[B]): Stream[(A, B)] = unfold((this, bs)) {
      case (Cons(ah, atl), Cons(bh, btl)) => Some(((ah(), bh()), (atl(), btl())))
      case _ => None
    }

    def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, bs)) {
      case (Cons(ah, atl), Cons(bh, btl)) => Some(((Some(ah()), Some(bh())), (atl(), btl())))
      case (Cons(ah, atl), Empty) => Some(((Some(ah()), None), (atl(), Empty)))
      case (Empty, Cons(bh, btl)) => Some(((None, Some(bh())), (Empty, btl())))
      case _ => None
    }

    // 5.14
    def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(_._2.nonEmpty).forAll { case (h1, h2) => h1 == h2 }

    // 5.15
    def tails: Stream[Stream[A]] = unfold(this) {
      case s@Cons(_, tl) => Some((s, tl()))
      case _ => None
    } append Stream(empty)

    // 5.16
    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
      case Empty => cons(z, empty)
      case Cons(h, tl) =>
        lazy val acc @ Cons(h1, _) = tl().scanRight(z)(f)
        cons(f(h(), h1()), acc)
    }

    def tailsWithScanRight: Stream[Stream[A]] = scanRight(empty[A])(cons(_, _))
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = cons(1, ones)

    // 5.8
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // 5.9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    // 5.10
    def fibs: Stream[Int] = {
      def _fibs(curr: Int, next: Int): Stream[Int] = cons(curr, _fibs(next, curr + next))

      _fibs(0, 1)
    }

    // 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z).fold(empty[A]) { case (a, s) =>
        cons(a, unfold[A, S](s)(f))
      }
    }

    // 5.12
    def fibsWithUnfold: Stream[Int] = unfold((0, 1)) { case (curr, next) => Some((curr, (next, curr + next))) }

    def fromWithUnfold(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

    def constantWithUnfold[A](a: A): Stream[A] = unfold(a)(a => Some((a, a)))

    def onesWithUnfold: Stream[Int] = unfold(1)(s => Some((s, s)))

  }

}
