package it.twinsbrain.fpinscala.chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldRight(ds,1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](aList: List[A]): List[A] = aList match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](a: A, aList: List[A]): List[A] = aList match {
    case Nil => Nil
    case Cons(_, t) => Cons(a, t)
  }

  def drop[A](n: Int, l: List[A]): List[A] = n match {
    case 0 => l
    case _ => drop(n - 1, tail(l))
  }

  def dropWhile[A](p: A => Boolean, l: List[A]): List[A] = l match {

    case Cons(h, t) => {
      if (p(h)) dropWhile(p, t)
      else l
    }
    case Nil => Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case _ => Nil
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc: Int) => acc + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as),z)((b,a) => f(a,b))

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]()) ((acc, e) => Cons(e, acc))

  def append[A](as:List[A], a: A): List[A] =
    foldLeft(reverse(as), List(a)) ((acc, e) => Cons(e, acc))

  def filter[A](as: List[A])(p: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((x, xs) => if(p(x)) Cons(x,xs) else xs)

  def flatten[A](as: List[List[A]]): List[A] = {
    foldLeft(as, List[A]()) ((acc, list) => foldLeft(list,acc)(append))
  }

  def addOne(as: List[Int]): List[Int] = map(as)(_ + 1)

  def doublesToStr(as: List[Double]): List[String] = map(as)(_.toString)

  def map[A,B](as: List[A])(f: A => B):List[B] =
    foldRight(as, Nil:List[B])((x:A,xs:List[B]) => Cons(f(x),xs))
}