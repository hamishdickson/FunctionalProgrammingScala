package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

/**
  * RNG: a purely functional random number generator
  */
case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  /**
   * returns a pair of the same number
   */
  def randomSamePair(rng: RNG): (Int, Int)  = {
    val (i1, _) = rng.nextInt
    val (i2, _) = rng.nextInt
    (i1, i2)
  }

  /**
   * returns a pair of random numbers
   */
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  /**
   * Exercise 6.1: Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inc)
   * Make sure to handle the corner case when nextInt returns Int.minValue, which doesn't have a non-neg counterpart
   *
   * Note: Their solution is a bit different than mine - they add one to neg numbers and * by -1
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, rng1) = rng.nextInt

    (i1, rng1) match {
      case (Int.MinValue, r) => (Int.MaxValue, r)
      case (v, r) => (Math.abs(v), r)
    }
  }
}
