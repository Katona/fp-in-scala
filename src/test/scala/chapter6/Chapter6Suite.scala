package chapter6

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class Chapter6Suite extends FunSuite {

  test("6.1 - nonNegativeInt") {
    val rng = SimpleRng(1)
    assert(Rng.nonNegativeInt(rng)._1 >=0)
    assert(Rng.nonNegativeInt(Rng.nonNegativeInt(rng)._2)._1 >=0)
  }

  test("6.2 - nextDouble") {
    val rng = SimpleRng(1)
    assert(Rng.nextDouble(rng)._1 == 2.140231792282322E-303)
  }

  test("6.4 - ints") {
    val rng = SimpleRng(1)
    val ints = Rng.ints(10)(rng)
    assert(ints._1.size == 10)
  }

  test("6.4 ints_2") {
    val rng = SimpleRng(1)
    val ints = Rng.ints_2(10)(rng)
    assert(ints._1.size == 10)
  }
}
