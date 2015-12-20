package chapter8

trait Prop {
  def check: Unit
  def &&(p: Prop): Prop = ???
}


object Gen {
/*  /**
    * Generates a list of type A
    */
  def listOf[A](a: Gen[A]): Gen[List[A]]

  /**
    * Creates n elements in a listOf
    */
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]*/
}