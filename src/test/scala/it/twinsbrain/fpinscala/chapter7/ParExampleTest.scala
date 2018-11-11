package it.twinsbrain.fpinscala.chapter7

import java.util.concurrent.Executors

import it.twinsbrain.fpinscala.chapter7.Par.Par
import org.scalatest._

class ParExampleTest extends FunSuite with Matchers {

  import ParExample._

  test("sum") {
    val parallelSum: Par[Int] = sum(Array(1, 2, 3))
    val es = Executors.newFixedThreadPool(4)
    Par.run(es)(parallelSum) shouldEqual 6
  }

  test("run function asynchronously") {
    val es = Executors.newFixedThreadPool(1)
    Par.run(es)(asyncF((n: Int) => n * n)(2)) shouldEqual 4
  }

  test("parMap") {
    val es = Executors.newFixedThreadPool(5)
    val square: Int => Int = (x: Int) => x * x
    Par.run(es)(parMap(List(1, 2, 3, 4))(square)) shouldEqual List(1, 4, 9, 16)
  }


  test("parFilter") {
    val es = Executors.newFixedThreadPool(5)
    val even: Int => Boolean = (x: Int) => x % 2 == 0
    Par.run(es)(parFilter(List(1, 2, 3, 4))(even)) shouldEqual List(2, 4)
  }
}
