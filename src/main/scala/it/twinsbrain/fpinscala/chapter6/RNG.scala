package it.twinsbrain.fpinscala.chapter6

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

object RNG{
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextVal, nextGen) = rng.nextInt
    val nonNegativeNextVal = if(nextVal == Integer.MIN_VALUE) 0 else math.abs(nextVal)
    (nonNegativeNextVal, nextGen)
  }
}
