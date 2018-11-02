package it.twinsbrain.fpinscala.chapter7

trait Par[A]

object Par {
  def fork[A](a: => Par[A]): Par[A] = ???

  def unit[A](a: A): Par[A] = ???

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = ???

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
}

object ParExample {

  def sum(ints: IndexedSeq[Int]): Par[Int] = if (ints.length <= 1)
    Par.unit(ints.headOption getOrElse 0) else {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }
}