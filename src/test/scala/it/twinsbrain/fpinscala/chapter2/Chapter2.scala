package it.twinsbrain.fpinscala.chapter2

import org.scalatest._

import scala.annotation.tailrec

class Chapter2Test extends FunSuite with Matchers {
  test("fibonacci") {
    fib(0) should be(0)
    fib(1) should be(1)
    fib(2) should be(1)
    fib(3) should be(2)
    fib(4) should be(3)
    fib(5) should be(5)
    fib(6) should be(8)
  }

  def fib(n: Int): Int = {

    @tailrec
    def loop(last: Int, curr: Int, count: Int): Int = {
      if (count == n) curr
      else loop(curr, curr + last, count + 1)
    }

    if (n <= 1) n
    else loop(0, 1, 1)
  }
}
