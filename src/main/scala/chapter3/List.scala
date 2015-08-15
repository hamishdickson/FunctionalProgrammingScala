package chapter3

/**
 * sealed trait means all implementations must be declared in the file
 */
sealed trait List[+A] // note: the plus means covariant

/**
 * covarience: if X is a subtype of Y => List[X] is a subtype of List[Y]
 */

/**
 * these two implementations are called "data constructors"
 */
case object Nil extends List[Nothing] // an empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // a non-empty list

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
   * Exercise 3.2: Implement the function tail for removing the first element of a list. Note that the function takes
   * constant time. What are the different choices you can make in your implementation if the list is Nil?
   */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil // could just as well throw an exception here
    case Cons(_, t) => t // _ is important here, it implies you don't care - it's a good practice thing
  }

  /**
   * Exercise 3.3: Using the same idea, implement the function setHead for replacing the first element of a list
   * with a different value
   */
  def setHead[A](l: List[A], v: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(v, t)
  }
}
