package chapter3


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Excercise2 {

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(h, t) => t
  }
}

object Excercise3 {

  def drop[A](list: List[A], n: Int): List[A] = list match {
    case Nil => Nil
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case l: List[A] => l
  }

  def setHead[A](list: List[A], newHead: A): List[A] = list match {
    case Nil => Nil
    case Cons(_, t) => Cons(newHead, t)
  }
}

object Excercise4 {
  def dropWhile[A](list: List[A])(predicate: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(h, t) if predicate(h) => dropWhile(t)(predicate)
    case l: List[A] => l
  }
}

object Excercise5 {
  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
    case Cons(h, t) => Cons(h, init(t))
  }
}
