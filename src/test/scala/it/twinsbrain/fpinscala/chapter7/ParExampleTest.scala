package it.twinsbrain.fpinscala.chapter7

import java.util.concurrent.{Executors, TimeUnit}

import it.twinsbrain.fpinscala.chapter7.Par.Par
import org.scalatest._

class ParExampleTest extends FunSuite with Matchers{
  import ParExample._

  test("sum"){
    val parallelSum: Par[Int] = sum(Array(1, 2, 3))
    val es = Executors.newFixedThreadPool(4)
    parallelSum(es).get(6, TimeUnit.SECONDS)  shouldEqual 6
  }

}
