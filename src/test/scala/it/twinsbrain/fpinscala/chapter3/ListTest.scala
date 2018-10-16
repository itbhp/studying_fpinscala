package it.twinsbrain.fpinscala.chapter3

import org.scalatest._

class ListTest extends FunSuite with Matchers{

 import List._

  test("tail on nonEmpty"){
    val aList = List(1,2)
    tail(aList) shouldEqual List(2)
  }

  test("tail on singleton"){
    val aList = List(1)
    tail(aList) shouldEqual Nil
  }

  test("tail on empty"){
    val aList = List()
    tail(aList) shouldEqual Nil
  }

  test("setHead on nonEmpty"){
    val aList = List(1,2)
    setHead(3, aList) shouldEqual List(3,2)
  }

  test("setHead on singleton"){
    val aList = List(1)
    setHead(3, aList) shouldEqual List(3)
  }

  test("setHead on empty"){
    val aList = List()
    setHead(4,aList) shouldEqual Nil
  }

}
