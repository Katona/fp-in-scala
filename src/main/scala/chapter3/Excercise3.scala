package chapter3

object Excercise3 {

  def drop[A](list: List[A], n: Int): List[A] = list match {
    case Nil => Nil
    case Cons(h, t) if n > 0 => drop(t, n - 1)
    case l: List[A]  => l
  }

  def dropWhile[A](list: List[A])(predicate: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(h, t) if predicate(h) => dropWhile(t)(predicate)
    case l: List[A] => l
  }
}
