package it.twinsbrain.fpinscala.chapter6

import it.twinsbrain.fpinscala.chapter6.StateOps.State
import org.scalatest._
import Machine._

class CandyMachineTest extends FunSuite with Matchers {

  val LOCKED = true
  val UNLOCKED = false

  test("Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.") {
    val initialCandies = 3
    val initialCoins = 5
    val initialMachine = Machine(LOCKED, initialCandies, initialCoins)
    val changesFor: State[Machine, (Int, Int)] = simulateMachine(List(Coin))

    val (_, newMachine) = changesFor(initialMachine)

    newMachine shouldBe Machine(UNLOCKED, initialCandies, initialCoins + 1)

  }

  test("Turning the knob on an unlocked machine will cause it to dispense candy and become locked.") {
    val initialCandies = 3
    val initialCoins = 5
    val initialMachine = Machine(UNLOCKED, initialCandies, initialCoins)
    val changesFor: State[Machine, (Int, Int)] = simulateMachine(List(Turn))

    val (_, newMachine) = changesFor(initialMachine)

    newMachine shouldBe Machine(LOCKED, initialCandies - 1, initialCoins)
  }

  test("Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.") {
    val initialCandies = 3
    val initialCoins = 5
    val lockedMachine = Machine(LOCKED, initialCandies, initialCoins)
    val unLockedMachine = Machine(UNLOCKED, initialCandies, initialCoins)
    for (m <- Seq((lockedMachine, Turn), (unLockedMachine, Coin))) {
      val (machine, input) = m
      val (_, newMachine) = simulateMachine(List(input))(machine)
      newMachine shouldBe machine
    }
  }

  test("A machine that’s out of candy ignores all inputs.") {
    val initialCandies = 3
    val initialCoins = 5
    val lockedMachine = Machine(LOCKED, 0, initialCoins)
    val unLockedMachine = Machine(UNLOCKED, 0, initialCoins)
    for (m <- Seq((lockedMachine, List(Turn,Turn,Coin)), (unLockedMachine, List(Coin,Coin,Turn)))) {
      val (machine, inputs) = m
      val (_, newMachine) = simulateMachine(inputs)(machine)
      newMachine shouldBe machine
    }
  }
}
