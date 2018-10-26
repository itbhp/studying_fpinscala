package it.twinsbrain.fpinscala.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}
