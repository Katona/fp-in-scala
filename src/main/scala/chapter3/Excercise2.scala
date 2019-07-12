package chapter3

object Excercise2 {

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(h, t) => t
  }
}
