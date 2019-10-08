package chapter5

import scala.collection.mutable.ListBuffer

trait Stream[+A] {
    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
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
        return if (as.isEmpty) empty else  cons(as.head, apply(as.tail: _*))
    }

}