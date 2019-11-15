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

  def nextDouble(rng: Rng): (Double, Rng) = {
    val (n, nextRng) = rng.nextInt
    (n.toDouble / Double.MaxValue, nextRng)
  }

  def intDouble(rng: Rng): ((Int,Double), Rng) = {
    val (i, n) = rng.nextInt
    val (d, n2) = Rng.nextDouble(n)
    ((i, d), n2)
  }
  def doubleInt(rng: Rng): ((Double,Int), Rng) = {
    val (d, n) = Rng.nextDouble(rng)
    val (i, n2) = n.nextInt
    ((d, i), n2)
  }
  def double3(rng: Rng): ((Double,Double,Double), Rng) = {
    val (d1, n) = Rng.nextDouble(rng)
    val (d2, n2) = Rng.nextDouble(n)
    val (d3, n3) = Rng.nextDouble(n2)
    ((d1, d2, d3), n3)
  }

  def ints(count: Int)(rng: Rng): (List[Int], Rng) = {
    (1 to count).foldLeft((List[Int](), rng)) {
      case ((l, r), _) => {
        val (i, nextRng) = r.nextInt
        val nextList: List[Int] = l :+ i
        (nextList, nextRng)
      }
    }
  }

  /**
   * Alternative implementation based on Stream.unfold.
   */
  def ints_2(count: Int)(rng: Rng): (List[Int], Rng) = {
    // We need to keep track the Rng so that we can return it at the end.
    var localRng = rng
    val ints = chapter5.Stream.unfold[Int, Rng](rng)(rng => {
      val (i, rng2) = rng.nextInt
      localRng = rng
      Some(i, rng2)
    }).take(count).toList
    (ints, localRng)
  }
}
