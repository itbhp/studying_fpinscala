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