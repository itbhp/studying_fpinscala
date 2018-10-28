package it.twinsbrain.fpinscala.chapter6

import scala.annotation.tailrec

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

  def int: Rand[Int] = _.nextInt

  def nonNegativeInt: Rand[Int] = map(int)(a => if (a == Integer.MIN_VALUE) 0 else math.abs(a))

  def double: Rand[Double] = map(nonNegativeInt) { nextVal =>
    if (nextVal == 0) 0
    else (Integer.MAX_VALUE - nextVal.toDouble) / Integer.MAX_VALUE
  }

  def intDouble: Rand[(Int, Double)] = rng => {
    val (nextInt, nextRNG) = nonNegativeInt(rng)
    val (nextDouble, otherNextRNG) = double(nextRNG)
    ((nextInt, nextDouble), otherNextRNG)
  }

  def doubleInt: Rand[(Double, Int)] = rng => {
    val (nextInt, nextRNG) = nonNegativeInt(rng)
    val (nextDouble, otherNextRNG) = double(nextRNG)
    ((nextDouble, nextInt), otherNextRNG)
  }

  def double3: Rand[(Double, Double, Double)] = rng => {
    val (aValue, aRng) = double(rng)
    val (bValue, bRng) = double(aRng)
    val (cValue, cRng) = double(bRng)
    ((aValue, bValue, cValue), cRng)
  }

  def ints(count: Int): Rand[List[Int]] = rng => {
    @tailrec
    def loop(c: Int, acc: (List[Int], RNG)): (List[Int], RNG) = {
      if (c > 0) {
        val (curList, curRNG) = acc
        val (nextInt, nextRNG) = curRNG.nextInt
        loop(c - 1, (nextInt :: curList, nextRNG))
      } else acc
    }

    loop(count, (Nil, rng))
  }
}
