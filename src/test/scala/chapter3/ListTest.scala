package chapter3

import org.scalatest._

class ListTest extends FlatSpec with Matchers {
  "A list" should "drop the correct elements" in {
    val xs: List[Int] = List(1, 2, 3, 4, 5)
    val ex1 = List.dropWhile(xs, (x: Int) => x < 4)

    ex1 should be (List(4, 5))
  }

}