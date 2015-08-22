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

  /**
   * Exercise 3.4: Generalize tail to the function drop, which removes the first n elements from a list. Note that this
   * function takes time proportional only to the number of elements being dropped - we don't need to make a cope of the
   * entire list
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  /**
   * Exercise 3.5: Implement dropWhile, which removes elements from the List prefix as long as they match a predicate
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  /**
   * Interestingly, to use dropWhile above you need to specify the type in the function, but if you curry the function
   * you don't need to. (It's a compilier thing)
   */
  def dropWhileCurry[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  /**
   * Exercise 3.6: Implement a function init, that returns a list consisting of all but the last element of a list. So
   * given a list(1, 2, 3, 4), init will return a list List(1, 2, 3). Why can't this function be implemented in
   * constant time tail?
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
}
