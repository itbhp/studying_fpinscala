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
      value.map2(Right(0.0))((a,b) => a.toDouble + b) shouldEqual Left("enough!")
    }

    withClue("Right") {
      Right(4).map2(Right(0.0))((a,b) => a.toDouble + b) shouldEqual Right(4.0)
    }
  }
}
