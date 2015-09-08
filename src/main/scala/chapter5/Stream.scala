package chapter5

sealed trait Stream[+A]
case object Empty extends Stream[Nothing]

/**
 * a non-empty stream consists of a head and a tail, which are both non-strict. Due to technical limitations, these
 * are thunks that must be explicitly forces, rather than by-name parameters
 */
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  /**
   * A smart constructor for creating a non-empty stream
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