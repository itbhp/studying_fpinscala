package it.twinsbrain.fpinscala.chapter5

import org.scalatest._

class StreamTest extends FunSuite with Matchers{

  import Stream._
  test("toList"){
    cons(1,cons(2,cons(3,cons(4,Stream.empty)))).toList shouldEqual List(1,2,3,4)
  }
}
