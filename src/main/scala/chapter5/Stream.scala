package chapter5

sealed trait Stream[+A] {

  /**
   * Exercise 5.1: write a helper function to convert the Stream to a list
   */
  def toList: List[A] = this match {
    case Empty => List();
    case Cons(h, t) => h() :: t().toList // don't know why I need brackets here?
  }


  def toListTheirSolution: List[A] = this match {
    case Cons(h, t) => h() :: t().toList // don't know why I need brackets here?
    case _ => List(); // ? what else is there than empty?
  }
}

case object Empty extends Stream[Nothing]

/**
 * a non-empty stream consists of a head and a tail, which are both non-strict. Due to technical limitations, these
 * are thunks that must be explicitly forces, rather than by-name parameters
 */
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  /**
   * A smart constructor for creating a non-empty stream
   *
   * Typcially want to cache the values of a Cons node, once they are forced. If we use the Cons data constructor
   * directly code can be computed twice.
   *
   * Avoid this problem with smart constructors => a function for constructing a data type other than the one used
   * in pattern matching
   *
   * By convention lower case names
   *
   * Here the cons smart constructor takes care of memorizing the by-name args for the head and tail of the Cons. This
   * is a common trick and ensures our thunk only does the work once. Subsequent calls use the cached lazy val
   */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // cache head and tail as lazy to avoid repeat calc
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /**
   * A smart constructor for empty
   */
  def empty[A]: Stream[A] = Empty

  /**
   * A convenient variable-arg method for constructing a stream from multiple elements
   */
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}