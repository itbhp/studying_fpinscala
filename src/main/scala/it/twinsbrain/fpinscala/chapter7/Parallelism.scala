package it.twinsbrain.fpinscala.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((a, _) => f(a))

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(
      new Callable[A] {
        def call = a(es).get
      }
    )

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
  //    es => UnitFuture(ps.map(parElem => parElem(es).get))
    ps.foldRight[Par[List[A]]](unit(List()))((parElem, acc) => map2(parElem, acc)(_ :: _))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  case class Map2Future[A, B, C](a: Future[A], b: Future[B],
                                 f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    def isDone = cache.isDefined

    def isCancelled = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    def get = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime;
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

}

object ParExample {

  import Par._

  def asyncF[A, B](f: A => B): A => Par[B] = a => map(lazyUnit(a))(f)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = sequence(ps.map(asyncF(f)))

  //    ps match {
  //      case Nil => lazyUnit(Nil)
  //      case x :: xs => map2(lazyUnit(f(x)), fork(parMap(xs)(f)))(_ :: _)
  //    }

  def sum(ints: IndexedSeq[Int]): Par[Int] = if (ints.length <= 1)
    Par.unit(ints.headOption getOrElse 0) else {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }
}