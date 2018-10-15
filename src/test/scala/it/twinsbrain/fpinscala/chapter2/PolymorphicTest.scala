package it.twinsbrain.fpinscala.chapter2

import org.scalatest._

class PolymorphicTest extends FunSuite with Matchers {

  test("isSorted") {
    val fromLesserToGreater: (Int, Int) => Boolean = (prev: Int, curr: Int) => prev < curr
    withClue("Array(2,3,1)") {
      isSorted(Array(2,3,1), fromLesserToGreater) should be(false)
    }
    withClue("Array(1,2,3)") {
      isSorted(Array(1,2,3), fromLesserToGreater) should be(true)
    }
  }

  def isSorted[A](collection: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(currentIndex: Int):Boolean = {
      if(currentIndex > collection.length - 1) true
      else if(!ordered(collection(currentIndex - 1), collection(currentIndex))) false
      else loop(currentIndex + 1)
    }
    if(collection.length < 2) true
    else loop( 1)
  }

}
