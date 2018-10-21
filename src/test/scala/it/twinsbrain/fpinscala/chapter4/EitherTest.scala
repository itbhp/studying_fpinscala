package it.twinsbrain.fpinscala.chapter4

import org.scalatest._

class EitherTest extends FunSuite with Matchers {

  test("map") {
    withClue("Left") {
      val value: Either[Int, Int] = Left(4)
      value.map(_ * 2) shouldEqual Left(4)
    }

    withClue("Right") {
      Right(4).map(_ * 2) shouldEqual Right(8)
    }
  }

  test("flatMap") {
    withClue("Left") {
      val value: Either[Int, Int] = Left(4)
      value.flatMap(v => Right(v % 2)) shouldEqual Left(4)
    }

    withClue("Right") {
      Right(4).flatMap(v => Right(v % 2 == 0)) shouldEqual Right(true)
    }
  }

  test("orElse") {
    withClue("Left") {
      val value: Either[Int, Int] = Left(4)
      value.orElse(Right(0)) shouldEqual Right(0)
    }

    withClue("Right") {
      Right(4).orElse(Right(0)) shouldEqual Right(4)
    }
  }

  test("map2") {
    withClue("Left") {
      val value: Either[String, Int] = Left("enough!")
      value.map2(Right(0.0))((a, b) => a.toDouble + b) shouldEqual Left("enough!")
    }

    withClue("Right") {
      Right(4).map2(Right(0.0))((a, b) => a.toDouble + b) shouldEqual Right(4.0)
    }
  }

  import Either._

  test("sequence on Nil") {
    sequence(Nil) shouldEqual Right(Nil)
  }

  test("sequence with Lefts") {
    sequence(List(Right(4), Right(5), Left("1"), Right(3), Left("2"))) shouldEqual Left("1")
  }

  test("sequence with Rights only") {
    sequence(List(Right(4), Right(5), Right(3))) shouldEqual Right(List(4, 5, 3))
  }

  val f: Int => Either[Exception, Int] = i => Try(1 / i)

  test("traverse on Nil") {
    traverse(Nil)(f) shouldEqual Right(Nil)
  }

  test("traverse with no Exception") {
    traverse(List(4, 5, 3))(f) shouldEqual Right(List(1 / 4, 1 / 5, 1 / 3))
  }

  test("traverse with Exception") {
    traverse(List(4, 5, 0, 3, 0))(f) match {
      case Right(_) => fail()
      case Left(e) => e shouldBe a [ArithmeticException]
    }
  }

}
