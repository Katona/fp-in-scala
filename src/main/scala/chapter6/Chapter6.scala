package chapter6

trait Rng {
  def nextInt: (Int, Rng)
}

case class SimpleRng(seed: Long) extends Rng {
  override def nextInt: (Int, Rng) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRng(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Rng {
  @scala.annotation.tailrec
  def nonNegativeInt(rng: Rng): (Int, Rng) = {
    val (randomNumber, newRng) = rng.nextInt
    if (randomNumber >= 0 && randomNumber != Int.MinValue) {
      (randomNumber, newRng)
    } else {
      nonNegativeInt(newRng)
    }
  }
}
