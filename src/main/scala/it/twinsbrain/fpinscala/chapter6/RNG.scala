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
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rc => {
      val (a, rng) = ra(rc)
      val (b, nextRng) = rb(rng)
      (f(a, b), nextRng)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng =>
    fs.foldRight((empty[A], rng))((ra: Rand[A], acc: (List[A],RNG)) => {
      val (list, rng) = acc
      val (nextVal, nextRng) = ra(rng)
      (nextVal :: list, nextRng)
    })

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (aVal, nextARng) = f(rng)
    g(aVal)(nextARng)
  }

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

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

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
