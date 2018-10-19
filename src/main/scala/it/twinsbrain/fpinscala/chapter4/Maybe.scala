package it.twinsbrain.fpinscala.chapter4

sealed trait Maybe[+A]{
  def map[B](f: A => B): Maybe[B] = this match {
    case None => None
    case Just(v) => Just(f(v))
  }
  def flatMap[B](f: A => Maybe[B]): Maybe[B] = ???
  def getOrElse[B >: A](default: => B): B = ??? // default is a by name parameter
  def orElse[B >: A](ob: => Maybe[B]): Maybe[B] = ???
  def filter(f: A => Boolean): Maybe[A] = ???
}

case class Just[+A](get: A) extends Maybe[A]

case object None extends Maybe[Nothing]