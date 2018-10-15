package it.twinsbrain.fpinscala.chapter2

import org.scalatest._

class CurryingTest extends FunSuite with Matchers{

  test("curry"){
    val product: (Int,Int) => Int = (a,b) => a * b

    curry(product)(2)(3) shouldEqual(product(2,3))
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  test("uncurry"){
    val productCurried: (Int) => (Int) => Int = a => b => a * b

    productCurried(2)(3) shouldEqual(uncurry(productCurried)(2,3))
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

}
