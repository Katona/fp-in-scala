package chapter5

/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Chapter5Suite extends FunSuite {

    test("5.1 - toList") {
        val stream = Stream(1, 2, 4, 5)
        val list = stream.toList
        assert(list == List(1, 2, 4, 5))
    }

    test("5.1 - toListFast") {
        val stream = Stream(1, 2, 4, 5)
        val list = stream.toListFast
        assert(list == List(1, 2, 4, 5))
    }

    test("5.2 - take") {
        val stream = Stream(1, 2, 3, 4, 5)
        assert(Nil == stream.take(0).toList)
        assert(List(1, 2) == stream.take(2).toList)
    }

    test("5.2 - drop") {
        val stream = Stream(1, 2, 3, 4, 5)
        assert(List(1, 2, 3, 4, 5) == stream.drop(0).toList)
        assert(Nil == stream.drop(5).toList)
        assert(List(5) == stream.drop(4).toList)
    }

    test("5.3 - takeWhile") {
        val stream = Stream(1, 2, 3, 4, 5)
        assert(List(1, 2) == stream.takeWhile( _ < 3).toList)
        assert(Nil == stream.takeWhile(_ => false).toList)
    }

    test("exists") {
        val stream = Stream(1, 2, 3, 4, 5)
        assert(stream.exist(_ == 5))
        assert(!stream.exist(_ == 6))
    }

    test("5.4 - forAll") {
        val stream = Stream(1, 2, 3, 4, 5)
        assert(stream.forAll(_ < 6))
        assert(!stream.forAll(_ < 3))
    }

    test("5.5 - takeWhileWithFoldRight") {
        val stream = Stream(1, 2, 3, 4, 5)
        assert(List(1, 2) == stream.takeWhileWithFoldRight( _ < 3).toList)
        assert(Nil == stream.takeWhileWithFoldRight(_ => false).toList)
    }

    test("5.6 - headOption2") {
        val stream = Stream(1, 2)
        assert(stream.headOption2.contains(1))
        assert(Stream.empty.headOption2.isEmpty)
    }

    test("5.7 - map") {
        val stream = Stream(1, 2, 3, 4)
        assert(stream.map(_ * 2).toList == List(2, 4, 6, 8))
        assert(Stream.empty[Int].map(_ * 2).toList == Nil)
    }

    test("5.7 - filter") {
        val stream = Stream(1, 2, 3, 4)
        assert(stream.filter(_ > 2).toList == List(3, 4))
        assert(stream.filter(_ < 2).toList == List(1))
        assert(stream.filter(_ == 2).toList == List(2))
    }

    test("5.7 - append") {
        assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))
        assert(Stream(1, 2, 3).append(Stream.empty).toList == List(1, 2, 3))
    }

    test("5.7 - flatMap") {
        assert(Stream(1, 2, 3).flatMap(a => Stream(a, a)).toList == List(1, 1, 2, 2, 3, 3))
    }

    test("5.8 - constant") {
        val constant = Stream.constant(1)
        assert(constant.take(5).toList == List(1, 1, 1, 1, 1))
    }

    test("5.9 - from") {
        assert(Stream.from(10).take(5).toList == List(10, 11, 12, 13, 14))
    }

    test("5.10 - fibs") {
        assert(Stream.fibs().take(6).toList == List(1, 1, 2, 3, 5, 8))
    }
}
