package chapter5

trait Stream[+A] {
    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
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