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

    test("5.11 - unfold") {
        assert(Stream.unfold((0, 1)){ case (prevPrev, prev) => Some(prev, (prev, prev + prevPrev))}.take(5).toList == List(1, 1, 2, 3, 5))
        assert(Stream.unfold(1){
            case elem if (elem < 5) => Some((elem, elem + 1))
            case _ => None
        }.toList == List(1, 2, 3, 4))
    }

    test("5.12 - fibs, from, ones, constant") {
        assert(Stream.fibs_1().take(5).toList == List(1, 1, 2, 3, 5))
        assert(Stream.from_1(5).take(5).toList == List(5, 6, 7, 8, 9))
        assert(Stream.ones_1().take(5).toList == List(1, 1, 1, 1, 1))
        assert(Stream.constant_1(8).take(5).toList == List(8, 8, 8, 8, 8))
    }

    test("5.13 - map, take, takeWhile, zipWith, zipAll based on unfold") {
        assert(Stream(1, 2, 3).map_1(_ * 2).toList == List(2, 4, 6))
        assert(Stream(1, 2, 3, 4).take_1(2).toList == List(1, 2))
        assert(Stream(1, 2, 3, 4).takeWhile_1(_ < 3).toList == List(1, 2))
        assert(Stream(1, 2, 3).zipWith(Stream(3, 2, 1))(_ + _).toList == List(4, 4, 4))
        assert(Stream(1, 2, 3).zipWith(Stream(3, 2, 1, 0))(_ + _).toList == List(4, 4, 4))
        assert(Stream(1, 2).zipAllWith(Stream(3, 2, 1, 0))((_, _)).toList == List(
            (Some(1), Some(3)),
            (Some(2), Some(2)),
            (None, Some(1)),
            (None, Some(0))
            )
        )
        assert(Stream(1, 2).zipAll(Stream(3, 2, 1, 0)).toList == List(
            (Some(1), Some(3)),
            (Some(2), Some(2)),
            (None, Some(1)),
            (None, Some(0))
            )
        )

    }

    test("5.14 - startsWith") {
        assert(Stream(1, 2, 3).startsWith(Stream(1, 2)) == true)
        assert(Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)) == false)
        assert(Stream(1, 2, 3).startsWith(Stream(4, 2, 3)) == false)
    }
}
