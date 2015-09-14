package chapter5

sealed trait Stream[+A] {

  /**
   * Exercise 5.1: write a helper function to convert the Stream to a list
   */
  def toList: List[A] = this match {
    case Empty => List();
    case Cons(h, t) => h() :: t().toList // don't know why I need brackets here? (the h and t)
  }


  def toListTheirSolution: List[A] = this match {
    case Cons(h, t) => h() :: t().toList // don't know why I need brackets here? (the h and t)
    case _ => List() // ? what else is there than empty?
  }

  /**
   * Exercise 5.2: Write take(n) which takes the first n elements of s stream and drop(n) which drops the last
   * n elements of a stream
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /**
   * Exists - checks if an element exists in the stream
   */
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p) // note this bit - don't actually ever eval t() if p(h()) true
    case _ => false
  }

  /**
   * Lazy foldRight
   *
   * note z and the second parameter of f are now non-strict
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  /**
   * Exercise 5.4: Implement forAll which checks that all elements in the Stream match a given
   * predicate. The impl should terminate the traversal as soon as it encounters a non-matching value
   */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /**
   * Exercise 5.5: Use foldRight to implement takeWhile
   */
  def takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)
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