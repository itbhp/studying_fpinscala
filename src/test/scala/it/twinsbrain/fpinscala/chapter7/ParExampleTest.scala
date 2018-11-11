package it.twinsbrain.fpinscala.chapter7

import java.util.concurrent.Executors

import it.twinsbrain.fpinscala.chapter7.Par.Par
import org.scalatest._

class ParExampleTest extends FunSuite with Matchers {
  val es = Executors.newFixedThreadPool(1)

  import ParExample._

  test("sum") {
    val parallelSum: Par[Int] = sum(Array(1, 2, 3))
    Par.run(es)(parallelSum) shouldEqual 6
  }

  test("run function asynchronously") {
    Par.run(es)(asyncF((n: Int) => n * n)(2)) shouldEqual 4
  }

  test("parMap") {
    val square: Int => Int = (x: Int) => x * x
    Par.run(es)(parMap(List(1, 2, 3, 4))(square)) shouldEqual List(1, 4, 9, 16)
  }


  test("parFilter") {
    val even: Int => Boolean = (x: Int) => x % 2 == 0
    Par.run(es)(parFilter(List(1, 2, 3, 4))(even)) shouldEqual List(2, 4)
  }

  //FIXME
  ignore("run on exception") {
    val delayWithException: Int => Par[Int] = asyncF(x => {
      throw new IllegalArgumentException
      x
    })
    Par.run(es)(delayWithException(1)) shouldEqual 1
  }
}
