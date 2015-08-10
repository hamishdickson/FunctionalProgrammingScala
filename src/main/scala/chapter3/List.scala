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
}
