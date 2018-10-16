package it.twinsbrain.fpinscala.chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail(aList: List[Int]): List[Int] = aList match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead(a:Int, aList: List[Int]): List[Int] = aList match {
    case Nil => Nil
    case Cons(_, t) => Cons(a,t)
  }
}