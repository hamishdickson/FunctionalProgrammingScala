package chapter2

import org.scalatest._

class MyModule$Test extends FlatSpec with Matchers {
  "A MyModule" should "be able to check if an array is sorted" in {
    val array = new Array[Int](5)

    array(0) = 1
    array(1) = 2
    array(2) = 1
    array(3) = 1
    array(4) = 1

    def test(n: Int, m: Int): Boolean = {
      if (n > m) false
      else true
    }

    MyModule.isSorted(array, test) should be (false)
  }
}
