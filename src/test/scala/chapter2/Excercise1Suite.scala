/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Excercise1Suite extends FunSuite {
  test("isSorted") {
    assert(Excercise1.isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b));
  }

  test("isSorted false") {
    assert(Excercise1.isSorted(Array(1, 2, 2), (a: Int, b: Int) => a < b) == false);
  }

  test("isSorted short") {
    assert(Excercise1.isSorted(Array(1, 2), (a: Int, b: Int) => a < b));
    assert(Excercise1.isSorted(Array(1), (a: Int, b: Int) => a < b));
  }

  test("isSorted string") {
    assert(Excercise1.isSorted(Array("a", "b"), (a: String, b: String) => a < b));
  }
}
