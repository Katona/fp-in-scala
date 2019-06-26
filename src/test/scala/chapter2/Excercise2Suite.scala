/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Excercise2Suite extends FunSuite {
  test("partial1") {
    val onePlus = Excercise2.partial1(1, (a: Int, b: Int) => a + b);
    assert(onePlus(2) == 3)
    assert(onePlus(-2) == -1)
  }
}