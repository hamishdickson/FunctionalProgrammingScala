package chapter5

import org.scalatest.{Matchers, FlatSpec}

class StreamTest extends FlatSpec with Matchers {
  "A startsWith" should "erm ... work" in {
    val s: Stream[Int] = Stream(1, 2, 3)
    val t: Stream[Int] = Stream(1, 2)

    s startsWith(t) should be (true)
  }
}
