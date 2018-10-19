package it.twinsbrain.fpinscala.chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int =  {
    def loop(maxInt: Int, t:Tree[Int]): Int = {
     t match {
       case Leaf(value) => value max maxInt
       case Branch(left, right) => loop(maxInt, left) max loop(maxInt, right)
     }
    }
    loop(0, tree)
  }
}