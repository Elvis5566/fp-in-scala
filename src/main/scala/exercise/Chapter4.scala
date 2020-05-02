package exercise

object Chapter4 {

  object Option {

    // 4.1
    sealed trait Option[+A] {
      def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(get) => Some(f(get))
      }

      def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case None => None
        case Some(get) => f(get)
      }

      def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(get) => get
      }

      def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case None => ob
        case _ => this
      }

      def filter(f: A => Boolean): Option[A] = this match {
        case None => None
        case Some(get) => if (f(get)) this else None
      }
    }

    case class Some[+A](get: A) extends Option[A]

    case object None extends Option[Nothing]

    // 4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }

    // 4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      a.foldRight[Option[List[A]]](Some(Nil))((x, acc) => map2(x, acc)(_ :: _))
    }

    // 4.5
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case h :: tl => map2(f(h), traverse(tl)(f))(_ :: _)
    }
  }

  object Either {

    // 4.6
    sealed trait Either[+E, +A] {
      def map[B](f: A => B): Either[E, B] = this match {
        case l@Left(_) => l
        case Right(value) => Right(f(value))
      }

      def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case l@Left(_) => l
        case Right(value) => f(value)
      }

      def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Left(_) => b
        case r@Right(_) => r
      }

      def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
        case (Right(v1), Right(v2)) => Right(f(v1, v2))
        case (l@Left(_), _) => l
        case (_, l@Left(_)) => l
      }
    }

    case class Left[+E](value: E) extends Either[E, Nothing]

    case class Right[+A](value: A) extends Either[Nothing, A]

    // 4.7
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
      case Nil => Right(Nil)
      case h :: tl => for {
        hh <- h
        tt <- sequence(tl)
      } yield hh :: tt
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case h :: tl => for {
        hh <- f(h)
        tt <- traverse(tl)(f)
      } yield hh :: tt
    }
  }

}
