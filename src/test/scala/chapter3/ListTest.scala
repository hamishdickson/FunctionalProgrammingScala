package chapter3

import org.scalatest._

class ListTest extends FlatSpec with Matchers {
  "A list" should "drop the correct elements" in {
    val xs: List[Int] = List(1, 2, 3, 4, 5)
    val ex1 = List.dropWhile(xs, (x: Int) => x < 4)

    ex1 should be (List(4, 5))
  }

  /**
   * Exercise 3.8: What happens if you do the following (val xs)?
   *
   * Note: it gives this: Cons(1, Cons(2, Cons(3, Nil)))
   */
  "A list" should "do what it does when cons and nil are passed in" in {
    val xs = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_,_))

    print(xs)
  }

  /**
   * Exercise 3.9: Compute the length of a list using foldRight
   */
  "length" should "compute the length of a list" in {
    val as: List[Int] = List(1, 2, 3)
    val bs: List[Int] = List(1, 2, 3, 4, 5)

    val len: Int = List.length(as)
    val len2: Int = List.length(bs)

    len should be (3)
    len2 should be (5)
  }

}