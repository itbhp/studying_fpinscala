package it.twinsbrain.fpinscala.chapter7

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import scalaz.concurrent.Actor

object Par {

  sealed trait Future[A] {
    private[chapter7] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) {
      a => {
        ref.set(a)
        latch.countDown
      }
    }
    latch.await
    ref.get()
  }

  def unit[A](a: A): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit = cb(a)
  }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
  }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((a, _) => f(a))

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
    def apply(cb: C => Unit): Unit = {
      var ar: Option[A] = None
      var br: Option[B] = None
      val combiner = Actor[Either[A, B]] {
        case Left(a) => br match {
          case None => ar = Some(a)
          case Some(b) => eval(es)(cb(f(a, b)))
        }
        case Right(b) => ar match {
          case None => br = Some(b)
          case Some(a) => eval(es)(cb(f(a, b)))
        }
      }
      p(es)(a => combiner ! Left(a))
      p2(es)(b => combiner ! Right(b))
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((parElem, acc) => map2(parElem, acc)(_ :: _))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceMap(cond)(Map(true -> t, false -> f))
//    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val index = run(es)(n)
    (choices(index))(es)
  }

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = es =>{
    val keyVal = run(es)(key)
    choices(keyVal)(es)
  }
}

object ParExample {

  import Par._

  def asyncF[A, B](f: A => B): A => Par[B] = a => map(lazyUnit(a))(f)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(ps.map(asyncF(f)))
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map asyncF((a: A) => if (f(a)) List(a) else List())
    val value: Par[List[List[A]]] = sequence(pars)
    map(value)(_.flatten)
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
}