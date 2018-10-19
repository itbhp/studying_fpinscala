package it.twinsbrain.fpinscala.chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def fold[A, B](tree: Tree[A])(lMap: A => B)(bMap: (Tree[A], Tree[A]) => B): B = tree match {
    case Leaf(v) => lMap(v)
    case Branch(l, r) => bMap(l, r)
  }

  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l, r) => 1 + size(l) + size(r))

  def maximum(tree: Tree[Int]): Int =
    fold(tree)(identity)((l, r) => maximum(l) max maximum(r))

  def depth[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)((r, l) => 1 + (depth(l) max depth(r)))

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(map(l)(f), map(r)(f)))
}