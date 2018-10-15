package it.twinsbrain.fpinscala.chapter2

import org.scalatest.{FunSuite, Matchers}

class CompositionTest extends FunSuite with Matchers{

  test("compose functions same domains"){

    val productBy2 = (a:Int) => a * 2
    val productBy3 = (a:Int) => a * 3
    val productBy6 = (a:Int) => a * 6

    compose(productBy2, productBy3) (4) shouldEqual(productBy6(4))
  }

  test("compose functions different domains"){

    val doubleToStr: Double => String = (a: Double) => a.toString
    val inverse: Int => Double = (a:Int) => 1.0 / a
    val inverseAsString = (a:Int) => (1.0 / a).toString

    compose(doubleToStr, inverse) (4) shouldEqual(inverseAsString(4))
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

}
