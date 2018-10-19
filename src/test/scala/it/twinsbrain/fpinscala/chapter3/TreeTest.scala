package it.twinsbrain.fpinscala.chapter3

import org.scalatest._

class TreeTest extends FunSuite with Matchers{

  test("size on only a Leaf"){
    Tree.size(Leaf(3)) shouldEqual 1
  }

 test("size"){
   Tree.size(Branch(Leaf(1), Branch(Leaf(2),Leaf(3)))) shouldEqual 5
 }
}
