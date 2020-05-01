package exercise

import scala.annotation.tailrec

object Chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    // 3.2
    def tail[A](as: List[A]): List[A] = as match {
      case Nil => throw new Exception("Empty list")
      case Cons(_, tl) => tl
    }

    // 3.3
    def setHead[A](as: List[A], newHead: A): List[A] = as match {
      case Nil => Cons(newHead, Nil)
      case Cons(_, tl) => Cons(newHead, tl)
    }

    // 3.4
    def drop[A](l: List[A], n: Int): List[A] = {
      if (n <= 0)
        l
      else
        drop(tail(l), n - 1)
    }

    // 3.5
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, tl) =>
        if (f(h))
          dropWhile(tl)(f)
        else
          l
    }

    // 3.6
    def init[A](l: List[A]): List[A] = l match {
      case Nil => throw new Exception("Empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
      case Cons(h, tl) => Cons(h, init(tl))
    }

    // 3.9
    def length[A](as: List[A]): Int = {
      @tailrec
      def _length(as: List[A], n: Int): Int = as match {
        case Nil => n
        case Cons(h, tl) => _length(tl, n + 1)
      }

      _length(as, 0)
    }

    // 3.10
    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(h, tl) => foldLeft(tl, f(h, z))(f)
    }

    // 3.11
    def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

    def product(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

    // 3.12
    def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])(Cons(_, _))

    // 3.13
    def foldRightWithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)(f)

    // 3.14
    def appendWithFoldLeft[A](as: List[A], a: A): List[A] = reverse(Cons(a, reverse(as)))

    def appendWithFoldRight[A](as: List[A], a: A): List[A] = foldRight(as, Cons(a, Nil))(Cons(_, _))

    // 3.15
    def concatenateLists[A](ass: List[List[A]]): List[A] = reverse(foldLeft(ass, Nil: List[A])(foldLeft(_, _)(Cons(_, _))))

    // 3.16
    def add1ToIntList(as: List[Int]): List[Int] = reverse(foldLeft(as, Nil: List[Int]) { case (a, l) => Cons(a + 1, l) })

    // 3.17
    def listDoubleMapToString(as: List[Double]): List[String] = reverse(foldLeft(as, Nil: List[String]) { case (a, l) => Cons(a.toString, l) })

    // 3.18
    def map[A, B](as: List[A])(f: A => B): List[B] = {
      reverse(foldLeft(as, Nil: List[B]) { case (a, l) => Cons(f(a), l) })
    }

    // 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      reverse(foldLeft(as, Nil: List[A]) { case (a, l) =>
        if (f(a))
          Cons(a, l)
        else
          l
      })
    }

    // 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concatenateLists(map(as)(f))

    // 3.21
    def filterWithFlapMap[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as)(a => if (f(a)) List(a) else Nil)
    }

    @tailrec
    def _addCorrespondingElementsas[A, B](as: List[A], bs: List[B], acc: List[(A, B)]): List[(A, B)] = {
      (as, bs) match {
        case (Nil, _) | (_, Nil) => acc
        case (Cons(ah, atl), Cons(bh, btl)) =>
          _addCorrespondingElementsas(atl, btl, Cons((ah, bh), acc))

      }
    }

    // 3.22
    def addCorrespondingElements(as: List[Int], bs: List[Int]): List[Int] = {
      map(reverse(_addCorrespondingElementsas(as, bs, Nil)))(i => i._1 + i._2)
    }

    // 3.23
    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
      map(reverse(_addCorrespondingElementsas(as, bs, Nil)))(i => f(i._1, i._2))
    }

    // 3.24
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      @tailrec
      def _hasSubsequence(_sup: List[A], _sub: List[A]): Boolean = (_sup, _sub) match {
        case (_, Nil) => true
        case (Nil, Cons(_, _)) => false
        case (Cons(supHead, supTail), Cons(subHead, subTail)) =>
          if (supHead == subHead)
            _hasSubsequence(supTail, subTail)
          else
            _hasSubsequence(supTail, sub)
      }

      _hasSubsequence(sup, sub)
    }

    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    // 3.25
    def size[A](as: Tree[A]): Int = as match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    // 3.26
    def maximum(as: Tree[Int]): Int = as match {
      case Leaf(n) => n
      case Branch(left, right) => maximum(left) max maximum(right)
    }

    // 3.27
    def depth[A](as: Tree[A]): Int = as match {
      case Leaf(_) => 1
      case Branch(left, right) => (depth(left) max depth(right)) + 1
    }

    // 3.28
    def mapTree[A, B](as: Tree[A])(f: A => B): Tree[B] = as match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) => Branch(mapTree(left)(f), mapTree(right)(f))
    }

    // 3.28
//    def foldTree[A, B](as: Tree[A], z: B)(f: (A, B) => B): B = as match {
//      case Leaf(v) => f(v, z)
//      case Branch(left, right) =>
//        val l = foldTree(left, z)(f)
//        foldTree(right, l)(f)
//    }
  }


}
