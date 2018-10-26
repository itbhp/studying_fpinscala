package it.twinsbrain.fpinscala.chapter5

sealed trait Stream[+A] {

  override def toString: String = this match {
    case Empty => "empty"
    case Cons(h, t) => "{" + h().toString + "," + t().toString + "}"
  }

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

  import Stream._

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((elem, acc) => if (p(elem)) cons(elem, acc) else acc)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def append[B >: A](stream: => Stream[B]): Stream[B] =
    foldRight(stream)((elem, acc) => cons(elem, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((elem, acc) => f(elem).append(acc))

  def map[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def take(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), count) if count > 0 => Some(h(), (t(), count - 1))
      case _ => None
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case _ => None
    }

  def startsWith[B >: A](s2: Stream[B]): Boolean = this.zipAll(s2).takeWhile(_._2.isDefined).forAll {
    case (h1, h2) => h1 == h2
  }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h, t) => Some(Cons(h, t), t())
      case Empty => None
    } append Stream(Empty)


  def exists(p: A => Boolean): Boolean = filter(p).headOption.exists(_ => true)

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z))((a, streamAcc) =>
      streamAcc.headOption.map(h => cons(f(a,h), streamAcc)).getOrElse(empty[B])
    )
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

  def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((as, bs)) {
      case (Empty, Empty) => None
      case (Cons(_, _), Empty) => None
      case (Empty, Cons(_, _)) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    }
}