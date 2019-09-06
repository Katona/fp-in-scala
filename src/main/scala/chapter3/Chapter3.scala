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

object Excercise7 {
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }
  def foldRightShortCircuit[A, B](l: List[A], z: B, n: A)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) if h != n => f(h, foldRightShortCircuit(t, z, n)(f))
    case Cons(h, t) if h == n =>
      println("short circuit", t)
      f(h, z)
  }
  def prod(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)
}

object Excercise10 {
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }
}

object Excercise11 {
  def sum(l: List[Int]): Int = Excercise10.foldLeft(l, 0)(_+_)
  def product(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(h, _) => Excercise10.foldLeft(l, 1)(_*_)
  }
  def length(l: List[Int]): Int = Excercise10.foldLeft(l, 0)((l, _) => l + 1)
}

object Excercise12 {
  def reverse[A](l: List[A]): List[A] = Excercise10.foldLeft(l, List[A]()){ (reversed: List[A], h: A) => Cons(h, reversed) }
}