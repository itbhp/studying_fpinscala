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
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) shouldEqual List(1,2,3)
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
    append(List(1,2),4) shouldEqual List(1,2,4)
  }

  test("flatten"){
    flatten(List(List(2,1), List(3,4))) shouldEqual List(2,1,3,4)
  }

  test("addOne"){
    addOne(List(3,4)) shouldEqual List(4,5)
  }

  test("doubles to strings"){
    doublesToStr(List(3.0,4.0)) shouldEqual List("3.0","4.0")
  }

  test("filter"){
    filter(List(1,2,3,4,5,6))(_%2 == 0) shouldEqual List(2,4,6)
  }

  test("add two int lists same length"){
    addList(List(1,2,3),List(4,5,6)) shouldEqual List(5,7,9)
  }

  test("add two int lists one Nil"){
    addList(Cons(1,Cons(2,Nil)),Nil) shouldEqual Nil
  }

  test("add two int lists different legnths"){
    addList(List(1,2),List(3,4,5)) shouldEqual List(4,6)
  }

  test("take on empty list"){
    take(4, Nil) shouldEqual Nil
  }

  test("take on non empty list"){
    take(4, List(1,2,3,4,5,6)) shouldEqual List(1,2,3,4)
  }

  test("try to take more on non empty list"){
    take(4, List(1,2,3)) shouldEqual List(1,2,3)
  }

  test("startsWith"){
    startsWith(List(1,2,3,4,5,6,7),List(1,2)) shouldEqual true
  }

  ignore("hasSubsequence"){
    hasSubsequence(List(1,2,3,4,5,6,7),List(3,4,5)) shouldEqual true
  }
}
