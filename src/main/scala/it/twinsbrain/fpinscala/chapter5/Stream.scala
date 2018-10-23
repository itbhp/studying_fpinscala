package it.twinsbrain.fpinscala.chapter5

sealed trait Stream[+A] {
  def toList: List[A] = {
    def go(as: Stream[A], acc: List[A]): List[A] = as match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }

    go(this, Nil).reverse
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((elem, acc) => if (p(elem)) Stream.cons(elem, acc) else acc)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def append[B >: A](stream: => Stream[B]): Stream[B] =
    foldRight(stream)((elem, acc) => Stream.cons(elem, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((elem, acc) => f(elem).append(acc))

  def map[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty: Stream[A])((a, b) => {
      if (p(a))
        Stream.cons(a, b)
      else Stream.empty
    })

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  def from(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def fibs(): Stream[Int] = unfold((0, 1)) { case (prev, curr) => Some(prev, (curr, prev + curr)) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map { case (value, state) => cons(value, unfold(state)(f)) }.getOrElse(empty)
}