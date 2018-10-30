package it.twinsbrain.fpinscala.chapter6

import scala.collection.immutable.List.empty

object StateOps {
  type State[S,+A] = S => (A,S)

  def unit[S,A](a: A): State[S,A] =
    s => (a, s)

  def flatMap[A, B, S](f: State[S, A])(g: A => State[S, B]): State[S, B] = initialState => {
    val (aValue, nextState) = f(initialState)
    g(aValue)(nextState)
  }

  def map[A, B, S](s: State[S,A])(f: A => B): State[S,B] = flatMap(s)(a => unit(f(a)))

  def map2[A, B, C, S](sa: State[S,A], sb: State[S,B])(f: (A, B) => C): State[S,C] =
    flatMap(sa)(a => map(sb)(b => f(a,b)))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](empty[A]))((sa, acc) => map2(sa, acc)(_ :: _))

}
