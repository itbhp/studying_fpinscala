package it.twinsbrain.fpinscala.chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((left_size, right_size) => 1 + left_size + right_size)

  def maximum(tree: Tree[Int]): Int =
    fold(tree)(identity)((leftMax, rightMax) => leftMax max rightMax)

  def depth[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)((leftDepth, rightDepth) => 1 + (leftDepth max rightDepth))

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(v => Leaf(f(v)): Tree[B])((lTree, rTree) => Branch(lTree, rTree))
}