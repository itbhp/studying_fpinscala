package it.twinsbrain.fpinscala.chapter6

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class RNGTest extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  import RNG._

  test("non Negative rng ") {
    forAll { n: Int =>
      val (nextVal, _) = nonNegativeInt(SimpleRNG(n))
      nextVal should be >= 0
      nextVal should be <= Integer.MAX_VALUE
    }
  }

  test("random doubles ") {
    forAll { n: Int =>
      val (nextVal, _) = double(SimpleRNG(n))
      nextVal should be >= 0.0
      nextVal should be < 1.0
    }
  }

}
