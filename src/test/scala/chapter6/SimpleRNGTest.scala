package chapter6

import org.scalatest.{Matchers, FlatSpec}

class SimpleRNGTest extends FlatSpec with Matchers {
  "A RNG" should "return the same number over and over again with the same input" in {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt

    val (n2, rng3) = rng2.nextInt

    val (n3, rng4) = rng.nextInt

    n1 should be (n3)
    rng2 should be (rng4)
  }
}
