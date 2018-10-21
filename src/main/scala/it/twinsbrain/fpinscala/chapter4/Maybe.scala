package it.twinsbrain.fpinscala.chapter4

sealed trait Maybe[+A]{
  def map[B](f: A => B): Maybe[B] = this match {
    case None => None
    case Just(v) => Just(f(v))
  }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match {
    case None => None
    case Just(v) => f(v)
  }

  // default is a by name parameter
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Just(v) => v
  }

  def orElse[B >: A](ob: => Maybe[B]): Maybe[B] = this match {
    case None => ob
    case Just(v) => this
  }

  def filter(p: A => Boolean): Maybe[A] = this match {
    case Just(v) if(p(v)) => Just(v)
    case _ => None
  }
}

case class Just[+A](get: A) extends Maybe[A]

case object None extends Maybe[Nothing]

object Maybe{
  def lift[A,B](f: A => B) : Maybe[A] => Maybe[B] = (a: Maybe[A]) => a.map(f)

  def map2[A,B,C](a: Maybe[A], b: Maybe[B])(f: (A,B) => C) : Maybe[C] =
    a.flatMap(valA => b.map(valB => f(valA,valB)))

  def sequence[A](xs: List[Maybe[A]]): Maybe[List[A]] =  traverse(xs)(identity)

  def traverse[A, B](xs: List[A])(f: A => Maybe[B]): Maybe[List[B]] = xs match {
    case Nil => Just(Nil)
    case y::ys => f(y).flatMap(b => traverse(ys)(f).map(cs => b::cs))
  }
}