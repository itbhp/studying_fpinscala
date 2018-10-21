package it.twinsbrain.fpinscala.chapter4

import org.scalatest._

class OptionTest extends FunSuite with Matchers{

  test("map on None"){
    None.map(_.toString) shouldEqual None
  }

  test("map on Just"){
    Just(2).map(_.toString) shouldEqual Just("2")
  }

  test("flatMap on None"){
    None.flatMap(_=>Just(2)) shouldEqual None
  }

  test("flatMap on Just"){
    Just(2).flatMap(b => Just(b.toString)) shouldEqual Just("2")
  }

  test("getOrElse on Just"){
    Just(2).getOrElse(0) shouldEqual 2
  }

  test("getOrElse on None"){
    None.getOrElse(0) shouldEqual 0
  }

  test("orElse on Just"){
    Just(2).orElse(Just(0)) shouldEqual Just(2)
  }

  test("orElse on None"){
    None.orElse(Just(0)) shouldEqual Just(0)
  }

  test("filter on Just true"){
    Just(2).filter(_%2==0) shouldEqual Just(2)
  }

  test("filter on Just false") {
    Just(2).filter(_ % 3 == 0) shouldEqual None
  }

  test("filter on None"){
    None.orElse(Just(0)) shouldEqual Just(0)
  }
  import Maybe._

  test("sequence on Nil"){
    sequence(List()) shouldEqual Just(List())
  }

  test("sequence without None"){
    sequence(List(Just(2),Just(3),Just(4))) shouldEqual Just(List(2,3,4))
  }

  test("sequence with None"){
    sequence(List(Just(2),None,Just(4))) shouldEqual None
  }

  test("traverse with Nil"){
    traverse(Nil)(Just(_)) shouldEqual Just(Nil)
  }

  test("traverse"){
    traverse(List(2, 3))(Just(_)) shouldEqual Just(List(2,3))
  }
}
