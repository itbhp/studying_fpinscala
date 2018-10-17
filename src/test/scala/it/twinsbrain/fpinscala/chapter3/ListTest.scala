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

  test("drop on nonEmpty"){
    val aList = List(1,2,3)
    drop(2, aList) shouldEqual List(3)
  }

  test("drop on singleton"){
    val aList = List(1)
    drop(3, aList) shouldEqual Nil
  }

  test("drop on empty"){
    val aList = List()
    drop(4,aList) shouldEqual Nil
  }

  test("dropWhile on nonEmpty"){
    val aList: List[Int] = List(2,4,6,3,4,5)
    val isEven: Int => Boolean = (x: Int) => x % 2 == 0
    dropWhile(isEven, aList) shouldEqual List(3,4,5)
  }

  test("init on empty"){
    init(List()) shouldEqual Nil
  }

  test("init on singletonList"){
    init(List(1)) shouldEqual Nil
  }

  test("init on nonEmpty list"){
    init(List(1,2,3,4)) shouldEqual List(1,2,3)
  }

  test("exercises"){
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) shouldEqual(List(1,2,3))
  }

  test("length on nonEmpty list"){
    List.length(List(1,2,3,4,5)) should be(5)
  }

  test("reverse empty"){
    reverse(List()) shouldEqual Nil
  }

  test("reverse singleton"){
    reverse(List(1)) shouldEqual List(1)
  }

  test("reverse nonEmpty"){
    reverse(List(1,2,3)) shouldEqual List(3,2,1)
  }

  test("append"){
    append(List(1,2),4) shouldEqual(List(1,2,4))
  }
}
