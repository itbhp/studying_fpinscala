package it.twinsbrain.fpinscala.chapter6

import scala.annotation.tailrec
import scala.collection.immutable.List.empty

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type State[S,+A] = S => (A,S)
  type Rand[A] = State[RNG, A]

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

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def int: Rand[Int] = _.nextInt

  def nonNegativeInt: Rand[Int] = map(int)(a => if (a == Integer.MIN_VALUE) 0 else math.abs(a))

  def double: Rand[Double] = map(nonNegativeInt) { nextVal =>
    if (nextVal == 0) 0
    else (Integer.MAX_VALUE - nextVal.toDouble) / Integer.MAX_VALUE
  }

  def intDouble: Rand[(Int, Double)] = map2(nonNegativeInt, double)((_, _))

  def doubleInt: Rand[(Double, Int)] = map2(double, nonNegativeInt)((_, _))

  def double3: Rand[(Double, Double, Double)] = rng => {
    val (aValue, aRng) = double(rng)
    val (bValue, bRng) = double(aRng)
    val (cValue, cRng) = double(bRng)
    ((aValue, bValue, cValue), cRng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) {
    i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) {
        unit(mod)
      }
      else {
        nonNegativeLessThan(n)
      }
    }
  }
}
