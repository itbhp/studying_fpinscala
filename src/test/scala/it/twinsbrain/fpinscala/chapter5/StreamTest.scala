package it.twinsbrain.fpinscala.chapter5

import org.scalatest._

class StreamTest extends FunSuite with Matchers{

  test("toList"){
    Stream(1,2,3,4).toList shouldEqual List(1,2,3,4)
  }

  test("take on empty"){
    Stream().take(2) shouldEqual Stream()
  }

  test("take proper"){
    Stream(1,2,3,4).take(2).toList shouldEqual List(1,2)
  }

  test("take more"){
    Stream(1,2,3,4).take(5).toList shouldEqual List(1,2,3,4)
  }

  test("drop on empty"){
    Stream().drop(2) shouldEqual Stream()
  }

  test("drop proper"){
    Stream(1,2,3,4).drop(2).toList shouldEqual List(3,4)
  }

  test("drop more"){
    Stream(1,2,3,4).drop(5).toList shouldEqual List()
  }
}
