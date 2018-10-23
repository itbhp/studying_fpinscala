package it.twinsbrain.fpinscala.chapter5

import org.scalatest._

class StreamTest extends FunSuite with Matchers {

  test("toList") {
    Stream(1, 2, 3, 4).toList shouldEqual List(1, 2, 3, 4)
  }

  test("take on empty") {
    Stream().take(2) shouldEqual Stream()
  }

  test("take proper") {
    Stream(1, 2, 3, 4).take(2).toList shouldEqual List(1, 2)
  }

  test("take more") {
    Stream(1, 2, 3, 4).take(5).toList shouldEqual List(1, 2, 3, 4)
  }

  test("drop on empty") {
    Stream().drop(2) shouldEqual Stream()
  }

  test("drop proper") {
    Stream(1, 2, 3, 4).drop(2).toList shouldEqual List(3, 4)
  }

  test("drop more") {
    Stream(1, 2, 3, 4).drop(5).toList shouldEqual List()
  }

  test("forAll") {
    Stream(2, 4, 6).forAll(_ % 2 == 0) shouldEqual true
    Stream(2, 3, 6).forAll(_ % 2 == 0) shouldEqual false
  }

  test("dropWhile on empty") {
    Stream.empty[Int].takeWhile(_ % 2 == 0).toList shouldEqual Nil
  }

  test("dropWhile failing fast") {
    Stream(1, 2, 3).takeWhile(_ % 2 == 0).toList shouldEqual Nil
  }

  test("dropWhile") {
    Stream(2, 4, 8, 10, 3, 6).takeWhile(_ % 2 == 0).toList shouldEqual List(2, 4, 8, 10)
  }

  test("headOption on Empty") {
    Stream.empty[Int].headOption shouldEqual None
  }

  test("headOption on non Empty") {
    Stream(1, 2, 3).headOption shouldEqual Some(1)
  }

  test("map on Empty") {
    Empty.map((x:Any) => x.toString) shouldEqual Empty
  }

  test("map on non Empty") {
    Stream(1, 2, 3, 4).map((x:Int) => x * 3).toList shouldEqual List(3, 6, 9, 12)
  }

  test("flatMap on Empty") {
    Empty.flatMap((x:Any) => Stream(x.toString)) shouldEqual Empty
  }

  test("flatMap on non Empty") {
    Stream(1, 2, 3, 4).flatMap((x:Int) => Stream(x * 3)).toList shouldEqual List(3, 6, 9, 12)
  }

  test("append stream to empty one"){
    Stream.empty[Int].append(Stream(1,2)).toList shouldEqual List(1,2)
  }

  test("append stream to non empty one"){
    Stream(3,4).append(Stream(1,2)).toList shouldEqual List(3,4,1,2)
  }

  test("filter empty stream"){
    Stream.empty[Int].filter(_%2 == 0).toList shouldEqual List()
  }

  test("filter non empty stream"){
    Stream(3,4).filter(_%2 == 0).toList shouldEqual List(4)
  }

  import Stream._

  test("constant infinite stream"){
    constant(1).take(3).toList shouldEqual List(1,1,1)
    constant(1).take(6).toList shouldEqual List(1,1,1,1,1,1)
  }

  test("natural numbers infinite stream"){
    from(1).take(3).toList shouldEqual List(1,2,3)
    from(4).take(6).toList shouldEqual List(4,5,6,7,8,9)
  }

  test("fibs infinite stream of fibonacci numbers"){
    fibs().take(3).toList shouldEqual List(0,1,1)
    fibs().take(6).toList shouldEqual List(0,1,1,2,3,5)
  }
}
