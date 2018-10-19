package it.twinsbrain.fpinscala.chapter3

import org.scalatest._

class TreeTest extends FunSuite with Matchers {

  test("size on only a Leaf") {
    Tree.size(Leaf(3)) shouldEqual 1
  }

  test("size") {
    Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldEqual 5
  }

  test("maximum in ints tree"){
    val example = Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Branch(Leaf(1), Leaf(2))))
    Tree.maximum(example) shouldEqual 5
  }
}
