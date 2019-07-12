package chapter3

/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Excercise3Suite extends FunSuite {

  test("drop") {
    assert(Excercise3.drop(Nil, 1) == Nil)
    assert(Excercise3.drop(List(1), 1) == Nil)
    assert(Excercise3.drop(List(1, 2, 3), 1) == List(2, 3))
    assert(Excercise3.drop(List(1, 2, 3, 4, 5), 2) == List(3, 4, 5))
  }

  test("dropWhile") {
    assert(Excercise3.dropWhile(Nil)(_ => true) == Nil)
    assert(Excercise3.dropWhile(List(1))(_ => true) == Nil)
    assert(Excercise3.dropWhile(List(1))(_ => false) == List(1))
    assert(Excercise3.dropWhile(List(1, 2, 3, 4, 5))(_ => true) == Nil)
    assert(Excercise3.dropWhile(List(1, 2, 3, 4, 5))(i => i < 3 ) == List(3, 4, 5))
  }
}