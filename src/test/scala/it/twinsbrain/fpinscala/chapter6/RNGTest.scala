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

  test("random int double pairs ") {
    forAll { n: Int =>
      val ((nextInt, nextDouble), _) = intDouble(SimpleRNG(n))
      nextInt should be >= 0
      nextInt should be <= Integer.MAX_VALUE
      nextDouble should be >= 0.0
      nextDouble should be < 1.0
    }
  }

  test("random double int pairs ") {
    forAll { n: Int =>
      val ((nextDouble, nextInt), _) = doubleInt(SimpleRNG(n))
      nextInt should be >= 0
      nextInt should be <= Integer.MAX_VALUE
      nextDouble should be >= 0.0
      nextDouble should be < 1.0
    }
  }

  test("random double 3-tuple ") {
    forAll { n: Int =>
      val ((a, b, c), _) = double3(SimpleRNG(n))
      for {v <- Seq(a, b, c)} {
        v should be >= 0.0
        v should be < 1.0
      }
    }
  }


  test("random list of ints") {
    forAll { n: Int =>
      val (list, _) = ints(5)(SimpleRNG(n))

      list.size shouldBe 5

      list.map(x => assert(list.count(y => y == x) == 1))
    }
  }

}
