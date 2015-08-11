package chapter3

object Datastructures {
  def main(args: Array[String]): Unit = {
    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
    val ex3: List[String] = Cons("a", Cons("b", Nil))


    /**
     * Exercise 3.1: What is the result of the following?
     */
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t) // not sure why this has to be List.sum
      case _ => 101
    }

    println("Val x: " + x)
  }

}
