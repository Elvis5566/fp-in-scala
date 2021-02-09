package exercise

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Chapter7 {
  type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def fork[A](a: => Par[A]): Par[A] = { es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = { es: ExecutorService =>
      val af = a(es)
      val bf = b(es)
      println("map2 thread: " + Thread.currentThread())
      UnitFuture(f(af.get, bf.get))
    }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    // 7.3
    //    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C)(timeout: Long, timeUnit: TimeUnit): Par[C] = { es: ExecutorService =>
    //      val af = a(es)
    //      val bf = b(es)
    //      UnitFuture(f(af.get(timeout, timeUnit), bf.get(timeout, timeUnit)))
    //    }

    // 7.4
    def asyncF[A, B](f: A => B): A => Par[B] = { a =>
      lazyUnit(f(a))
    }

    // 7.5
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
      case Nil => unit(Nil)
      case h :: tl => map2(h, sequence(tl))(_ :: _)
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    // 7.6
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
      val r: Par[List[Boolean]] = parMap(as)(f)
      map2(unit(as), r) { (a, b) =>
        (a zip b).filter(_._2).map(_._1)
      }
    }

    // 7.7
    // map(y)(id) == y
    // map(map(y)(g))(f) == map(y)(f compose g)

    // 7.11
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = { es =>
      val _n = n(es).get()
      choices(_n)(es)
    }

    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = { es =>
      val a = pa(es).get()
      choices(a)(es)
    }

    // 7.13
    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond) { b =>
      if (b) t
      else f
    }

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(n) { n =>
      choices(n)
    }

    def join[A](a: Par[Par[A]]): Par[A] = { es =>
      a(es).get()(es)
    }

    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

    def joinByFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)
  }

}
