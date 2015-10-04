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
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, rng1) = rng.nextInt

    (i1, rng1) match {
      case (v, r) if v < 0 => (Math.abs(v + 1), r)
      case (v, r) => (Math.abs(v), r)
    }
  }

  /**
    * Exercise 6.2: Write a function to generate a Double between 0 and 1, not inc 1. Note: you can use Int.maxValue
   * to obtain the max positive integer value, and you can use x.toDouble to convert an x: Int to Double
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i1, rng1) = nonNegativeInt(rng)

    (i1 / Int.MaxValue.toDouble, rng1)
  }

  /**
    * Exercise 6.3: Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
   * (Double, Double, Double) 3-tuple. You should be able to reuse the functions you've already written
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (d1, rng2) = double(rng1)

    ((i1,d1), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i1, d1), rng1) = intDouble(rng)

    ((d1,i1), rng1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)

    ((d1,d2,d3), rng3)
  }

  /**
    * Exercise 6.4: Write a function to generate a list of random integers
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List(), rng)
    else {
      val (x, r1)  = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }
  }

  /**
   * We seem to do this a lot - create a type alias to describe it better
   *
   * this is a "state action" -> something that transforms a state. Since it's repetitive to pass along a state ourselves
   * we want our combinators (combinations of state actions) to pass along the state
   */
  type Rand[+A] = RNG => (A, RNG)

  /**
   * with Rand, we can now turn methods such as nextInt into values of this type
   */
  val int: Rand[Int] = _.nextInt

  /**
   * passes the RNG state through without using it
   */
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  /**
   * map for transforming the output of a state action without modifying the state itself. Remember Rand[A] is just
   * a type alias for a function of type RNG => (A, RNG), so this is just a kind of function composition
   */
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /**
   * Exercise 6.5: Use map to reimplement double in a more elegant way
   */
  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / Int.MaxValue.toDouble)

  /**
   * Exercise 6.6: Write an implementation of map2 using the following signature. This function takes 2 actions
   * ra and rb and a function f for combining their results and returns a new action that combines them
   */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)

      (f(a,b), rng3)
    }

}
