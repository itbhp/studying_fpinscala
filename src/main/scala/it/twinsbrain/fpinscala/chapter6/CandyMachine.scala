package it.twinsbrain.fpinscala.chapter6

import StateOps._

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = m => {
    val newMachine = inputs.foldLeft(m)(handleTransition)
    ((newMachine.candies, newMachine.coins), newMachine)
  }

  private def handleTransition(m: Machine, i: Input) =
    i match {
      case Coin if m.candies == 0 => m
      case Turn if m.candies == 0 => m
      case Coin if m.locked => m.copy(locked = false, coins = m.coins + 1)
      case Turn if !m.locked => m.copy(locked = true, candies = m.candies - 1)
      case _ => m
    }
}
