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
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextVal, nextGen) = rng.nextInt
    val nonNegativeNextVal = if (nextVal == Integer.MIN_VALUE) 0 else math.abs(nextVal)
    (nonNegativeNextVal, nextGen)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nextVal, nextGen) = nonNegativeInt(rng)
    val nonNegativeNextVal =
      if (nextVal == 0) 0
      else (Integer.MAX_VALUE.toDouble - nextVal.toDouble) / Integer.MAX_VALUE.toDouble
    (nonNegativeNextVal, nextGen)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextInt, nextRNG) = nonNegativeInt(rng)
    val (nextDouble, otherNextRNG) = double(nextRNG)
    ((nextInt, nextDouble), otherNextRNG)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (nextInt, nextRNG) = nonNegativeInt(rng)
    val (nextDouble, otherNextRNG) = double(nextRNG)
    ((nextDouble, nextInt), otherNextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (aValue, aRng) = double(rng)
    val (bValue, bRng) = double(aRng)
    val (cValue, cRng) = double(bRng)
    ((aValue, bValue, cValue), cRng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
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
