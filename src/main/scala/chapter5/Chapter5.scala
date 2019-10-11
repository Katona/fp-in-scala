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

    def apply[A](as: A*): Stream[A] = {
        if (as.isEmpty) empty else  cons(as.head, apply(as.tail: _*))
    }

}