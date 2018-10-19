package it.twinsbrain.fpinscala.chapter4

import org.scalatest._

class OptionTest extends FunSuite with Matchers{

  test("map on None"){
    None.map(_.toString) shouldEqual None
  }

  test("map on Just"){
    Just(2).map(_.toString) shouldEqual Just("2")
  }

}
