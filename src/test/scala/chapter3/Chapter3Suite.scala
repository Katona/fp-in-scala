package chapter3

/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Chapter3Suite extends FunSuite {

  test("play around") {
    val x = Cons(1, Cons(2, Cons(3, Nil)))
    println(x)
    val y = List(1, 2, 3)
    println(y)

    val z = List(1, 2, 3) match {
      case Cons(_, t) => t
    }
    println(z)
  }

  test("test pattern matching") {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(e, Cons(2, Cons(4, _))) => e
      case Nil => 42
      case Cons(e, Cons(y, Cons(3, Cons(4, _)))) => e + y
      case _ => 101
    }

    assert(x == 3)
  }

  test("tail") {
    assert(Excercise2.tail(Nil) == Nil)
    assert(Excercise2.tail(List(1)) == Nil)
    assert(Excercise2.tail(List(1, 2, 3)) == List(2, 3))
  }

  test("setHead") {
    assert(Excercise3.setHead(List(1, 2), 5) == List(5, 2))
  }

  test("drop") {
    assert(Excercise3.drop(Nil, 1) == Nil)
    assert(Excercise3.drop(List(1), 1) == Nil)
    assert(Excercise3.drop(List(1, 2, 3), 1) == List(2, 3))
    assert(Excercise3.drop(List(1, 2, 3, 4, 5), 2) == List(3, 4, 5))
  }

  test("dropWhile") {
    assert(Excercise4.dropWhile(Nil)(_ => true) == Nil)
    assert(Excercise4.dropWhile(List(1))(_ => true) == Nil)
    assert(Excercise4.dropWhile(List(1))(_ => false) == List(1))
    assert(Excercise4.dropWhile(List(1, 2, 3, 4, 5))(_ => true) == Nil)
    assert(Excercise4.dropWhile(List(1, 2, 3, 4, 5))(_ => false) == List(1, 2, 3, 4, 5))
    assert(Excercise4.dropWhile(List(1, 2, 3, 4, 5))(i => i < 3 ) == List(3, 4, 5))
  }

  test("init") {
    assert(Excercise5.init(List(1, 2)) == List(1))
    assert(Excercise5.init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))
  }

  test("foldRight") {
    assert(Excercise7.foldRight(List(1, 2), 1)(_*_) == 2)
    assert(Excercise7.foldRightShortCircuit(List(0, 1, 2, 3), 1, 0)(_*_) == 0)
  }

  test("Excercise8") {
    assert(Excercise7.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_,_)) == List(1, 2, 3))
  }

  test("Excercise9 - length with foldRight") {
    assert(Excercise7.foldRight(List(1, 2, 3), 0)((_, l) => l + 1) == 3)
    assert(Excercise7.foldRight(List(1), 0)((_, l) => l + 1) == 1)
    assert(Excercise7.foldRight(Nil, 0)((_, l) => l + 1) == 0)
  }

  test("Excercise10") {
    assert(Excercise10.foldLeft(List[Int](), 0)(_+_) == 0)
    assert(Excercise10.foldLeft(List(1, 2), 0)(_+_) == 3)
  }

  test("foldRight vs foldLeft") {
    def sum = (a: Int, b: Int) => {
      println(a, b)
      a + b
    }
    println("foldRight")
    Excercise7.foldRight(List(1, 2, 3, 4), 0)(sum)
    println("foldLeft")
    Excercise10.foldLeft(List(1, 2, 3, 4), 0)(sum)
  }

  test("Excercise11 - functions based on foldLeft") {
    assert(Excercise11.sum(List[Int]()) == 0)
    assert(Excercise11.sum(List(1, 2)) == 3)

    assert(Excercise11.product(List()) == 0)
    assert(Excercise11.product(List(1, 2)) == 2)

    assert(Excercise11.length(List()) == 0)
    assert(Excercise11.length(List(1, 2, 3)) == 3)
  }

  test("Excercise12 - reverse") {
    assert(Excercise12.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  test("Excercise13 - foldLeft based on foldRight") {
    println("foldLeft based on foldRight")
    Excercise13.foldLeft(List(1, 2, 3), 0){ (f, a) => 
      println(f, a)
      0
    }
  }

  test("Excercise13 - foldRight based on foldLeft") {
    println("foldRight based on foldLeft")
    Excercise13.foldRight(List(1, 2, 3), 0){ (f, a) => 
      println(f, a)
      f + a
    }
  }

  test("Excercise13 - foldLeft2") {
    Excercise13.foldLeft2(List(1, 2, 3), 0) { (acc, a) =>
      println(acc, a)
      acc + a
    }
  }

  test("Excercise13 - foldRight2") {
    Excercise13.foldRight2(List(1, 2, 3), 0) { (a, acc) =>
      println(acc, a)
      acc + a
    }
  }

  test("Excercise14 - append based on foldRight") {
    assert(Excercise14.append(List(1, 2, 3), List(4)) == List(1, 2, 3, 4))
  }

  test("Excercise15 - flatMap") {
    assert(Excercise15.flatMap(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))
  }

  test("Excercise16 - increase all element by one") {
    assert(Excercise16.addOne(List(1, 2, 3)) == List(2, 3, 4))
  }

  test("Excercise17 - convert to string all elements") {
    assert(Excercise17.toString(List(1.1, 2.2, 3.3)) == List("1.1", "2.2", "3.3"))
  }

  test("Excercise18 - map") {
    assert(Excercise18.map(List(1, 2, 3))(_*2) == List(2, 4, 6))
  }

  test("Excercise19 - filter") {
    assert(Excercise19.filter(List(1, 2, 3, 4))(a => a % 2 == 0) == List(2, 4))
  }

  test("Excercise20 - flatMap") {
    assert(Excercise20.flatMap(List(1, 2, 3, 4))(a => if (a % 2 == 0) List(a) else Nil) == List(2, 4))
  }

  test("Excercise22 - zipWith") {
    assert(Excercise22.zipWith(List(1, 2, 3), List(2, 3, 4)) == List(3, 5, 7))
    assert(Excercise22.zipWith(List(1, 2), List(2, 3, 4)) == List(3, 5, 4))
  }

  test("Excercise23 - zipWith") {
    assert(Excercise23.zipWith(List(1, 2, 3), List(2, 3, 4))(_ + _) == List(3, 5, 7))
    assert(Excercise23.zipWith(List(1, 2), List(2, 3, 4))(_ + _) == List(3, 5, 4))
  }

  test("Excercise24 - hasSubsequence") {
    assert(Excercise24.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) == true)
    assert(Excercise24.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) == true)
    assert(Excercise24.hasSubsequence(List(1, 2, 3, 4), List(3, 4)) == true)
    assert(Excercise24.hasSubsequence(List(1, 2, 3, 4), List(5)) == false)
    assert(Excercise24.hasSubsequence(List(1, 2, 3, 4), List(2, 4)) == false)
    assert(Excercise24.hasSubsequence(List(1, 2, 3, 4), List(3, 4, 5)) == false)
  }

  test("Excercise25 - size") {
    assert(Excercise25.size(Leaf(2)) == 1)
    assert(Excercise25.size(Branch(Leaf(1), Leaf(2))) == 3)
    assert(Excercise25.size(Branch(Leaf(1), Branch(Leaf(1), Leaf(2)))) == 5)
  }
}