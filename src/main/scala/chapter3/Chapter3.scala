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

object Excercise13 {
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = Excercise7.foldRight(Excercise12.reverse(l), z){(a, b) => f(b, a)}
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = Excercise10.foldLeft(Excercise12.reverse(l), z){(b, a) => f(a, b)}

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => (acc: B) => g(f(acc, a)))(z)
  
  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => (acc: B) => g(f(a, acc)))(z)
}

object Excercise14 {
  def append[A](l: List[A], l2: List[A]) = Excercise7.foldRight(l, l2)((a, acc) => Cons(a, acc))
}

object Excercise15 {
  def flatMap[A](l: List[List[A]]) = Excercise7.foldRight(l, Nil:List[A])((a, acc) => Excercise14.append(a, acc))
}

object Excercise16 {
  def addOne(l: List[Int]): List[Int] = Excercise7.foldRight(l, Nil: List[Int])((a, acc) => Cons(a + 1, acc))
}

object Excercise17 {
  def toString(l: List[Double]): List[String] = Excercise7.foldRight(l, Nil: List[String])((a, acc) => Cons(a.toString, acc))
}

object Excercise18 {
  def map[A, B](l: List[A])(f: A => B): List[B] = 
    Excercise7.foldRight(l, Nil: List[B])((a, acc) => Cons(f(a), acc))
}

object Excercise19 {
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    Excercise7.foldRight(as, Nil: List[A])((a, acc) => if (f(a) == true) Cons(a, acc) else acc)
}

object Excercise20 {
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    Excercise7.foldRight(as, Nil: List[B])((a, acc) => Excercise14.append(f(a), acc))
}

object Excercise22 {
  def zipWith(l1: List[Int], l2: List[Int]): List[Int] = {
    val buf = new collection.mutable.ListBuffer[Int]
    def go(l1: List[Int], l2: List[Int]): Unit = (l1, l2) match {
      case (Cons(h1, Nil), Cons(h2, Nil)) => buf += h1 + h2 
      case (Cons(h1, Nil), Cons(h2, t2)) => buf += h1 + h2; go(t2, List(0))
      case (Cons(h1, t1), Cons(h2, Nil)) => buf += h1 + h2; go(t1, List(0)) 
      case (Cons(h1, t1), Cons(h2, t2)) => println(l1, l2); buf += h1 + h2; go(t1, t2)
    }
    go(l1, l2)
    List(buf.toList: _*)
  }
}

object Excercise23 {
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l1: List[A], l2: List[A]): Unit = (l1, l2) match {
      case (Nil, Nil) => 
      case (Cons(h1, t1), Nil) => buf += h1; go(t1, Nil)
      case (Nil, Cons(h2, t2)) => buf += h2; go(t2, Nil)
      case (Cons(h1, t1), Cons(h2, t2)) => buf += f(h1, h2); go(t1, t2)
    }
    go(l1, l2)
    List(buf.toList: _*)
  }
}

object Excercise24 {
  def hasSubsequence[A](origSup: List[A], origSub: List[A]): Boolean = {
    def go[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(hsup, tsup), Cons(hsub, tsub)) if (hsup == hsub) => go(tsup, tsub)
      case (Cons(hsup, tsup), Cons(hsub, tsub)) if (hsup != hsub) => go(tsup, origSub)
    }
    go(origSup, origSub)
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Excercise25 {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }
}

object Excercise26 {
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }
}