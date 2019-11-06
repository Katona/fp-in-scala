package chapter5

import scala.collection.mutable.ListBuffer

trait Stream[+A] {
    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, _) => Some(h())
    }

    def toList: List[A] = {
        def go(s: Stream[A], acc: List[A]): List[A] = s match {
            case Empty => acc
            case Cons(h, t) => go(t(), h() :: acc)
        }

        go(this, Nil).reverse
    }

    def toListFast: List[A] = {
        val buf = new ListBuffer[A]

        def go(s: Stream[A]): List[A] = s match {
            case Cons(h, t) =>
                buf += h()
                go(t())
            case Empty => buf.toList
        }

        go(this)
    }

    final def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
        case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
        case _ => Empty
    }

    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
        case Cons(_, t) if n > 0 => t().drop(n - 1)
        case _ => this
    }

    final def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
        case _ => Stream.empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def exist(p: A => Boolean): Boolean =
        foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
        foldRight(true)((a, b) => p(a) && b)

    def headOption2: Option[A] =
        foldRight[Option[A]](None)((a, _) => Some(a))

    def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = {
        foldRight[Stream[A]](Stream.empty){(a, b) =>
            if (p(a)) Stream.cons(a, b)
            else Stream.empty
        }
    }

    def map[B](mapper: A => B): Stream[B] = foldRight(Stream.empty[B])((h, t) => Stream.cons(mapper(h), t))

    def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => {
        if (p(h)) Stream.cons(h, t)
        else t
    })

    def append[B>:A](other: Stream[B]): Stream[B] = foldRight(other)((h, t) => Stream.cons(h, t))

    def flatMap[B](m: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((h, t) => m(h).append(t))

    def map_1[B](mapper: A => B): Stream[B] = Stream.unfold(this) {
        case Cons(h, t) => Some((mapper(h()), t()))
        case Empty => None
    }

    def take_1(n: Int): Stream[A] = Stream.unfold((this, n)) {
        case (Cons(h, t), n) if (n > 0) => Some((h(), (t(), n - 1)))
        case _ => None
    }

    def takeWhile_1(p: A => Boolean): Stream[A] = Stream.unfold(this) {
        case Cons(h, t) if p(h()) => Some((h(), t()))
        case _ => None
    }

    def zipWith[B, C](other: Stream[B])(c: (A, B) => C) = Stream.unfold((this, other)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((c(h1(), h2()), (t1(), t2())))
        case _ => None
    }

    def zipAllWith[B, C](other: Stream[B])(c: (Option[A], Option[B]) => C): Stream[C] = Stream.unfold((this, other)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(c(Some(h1()), Some(h2())), (t1(), t2()))
        case (Empty, Cons(h2, t2)) => Some(c(None, Some(h2())), (Empty, t2()))
        case (Cons(h1, t1), Empty) => Some(c(Some(h1()), None), (t1(), Empty))
        case _ => None
    }

    def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = zipAllWith(other)((_, _))

    def startsWith[A](s: Stream[A]): Boolean = zipAllWith(s)((_, _)).takeWhile(!_._2.isEmpty)forAll{
        case (a, b) => a == b
    }

    def tails: Stream[Stream[A]] = Stream.unfold(this) { 
        case Empty => None
        case current @ Cons(_, _) => Some((current, current.t()))
    }

    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this.tails.map(_.foldRight(z)(f))

    def scanRight_1[B](z: => B)(f: (A, => B) => B): Stream[B] = this.foldRight(Stream(z)) {
        case (current, scanned @ Cons(h, _)) => Stream.cons(f(current, h()), scanned)
    }

}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
case object Empty extends Stream[Nothing]

object Stream {
    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
        lazy val lazyH = h
        lazy val lazyT = t
        Cons[A](() => lazyH, () => lazyT)
    }

    def empty[A]: Stream[A] = Empty

    def constant[A](a: A): Stream[A] = {
        lazy val infiniteStream: Stream[A] = Stream.cons(a, infiniteStream)
        infiniteStream
    }

    def apply[A](as: A*): Stream[A] = {
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }

    def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

    def fibs(): Stream[Int] = {
        def nextFib(prevPrev: Int, prev: Int): Stream[Int] = Stream.cons(prev, nextFib(prev, prev + prevPrev))
        nextFib(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
        def go(s: S): Stream[A] = {
            val next: Option[(A, S)] = f(s)
            next match {
                case Some((a, s)) => Stream.cons(a, go(s))
                case None => Stream.empty
            }
        }
        go(z)
    }

    def fibs_1(): Stream[Int] = Stream.unfold((0, 1)){ case (prevPrev, prev) => Some(prev, (prev, prev + prevPrev))}

    def from_1(n: Int): Stream[Int] = Stream.unfold(n)(v => Some(v, v + 1))

    def ones_1(): Stream[Int] = Stream.unfold(1)(_ => Some(1, 1))

    def constant_1(c: Int): Stream[Int] = Stream.unfold(c)(c => Some(c, c))
}