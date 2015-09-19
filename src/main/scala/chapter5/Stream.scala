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
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

  /**
   * Exercise 5.6: Hard - implement headOption
   *
   * Got close - again, need the type on None
   */
  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  /**
   * Exercise 5.7: Implement map, filter, append, flatMap
   */
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a) append b)


  /**
   * Exercise 5.13: Use unfold to implement map, take, takeWhile, zipWith  and zipAll. zipAll should continue
   * the traversal as long as either stream has more elements - it uses Option to indicate whether each stream
   * has been exhausted
   */
  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)){
      case (Cons(h,t), 1) => Some((h(), (Stream.empty, 0)))
      case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h,t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWithViaUnfold[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  /**
   * Exercise 5.14: Hard. Implement startsWith using functions you've written. It should check if
   * one Stream is a prefix of another. For instance, Stream(1, 2, 3) startsWith Stream(1, 2) would be true
   */
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
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

  /**
   * Exercise 5.8: Generalise ones to the function constant, which returns an infinite Stream a given value
   */
  val ones: Stream[Int] = cons(1, ones)

  // my attempt
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // better since it doesn't eval stuff over and over
  def constantLazy[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /**
   * Exercise 5.9: Write a function that generalizes an infinite Stream of integers, starting from n
   */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
   * Exercise 5.10: Write a function fib, that generalises the infinite stream of fibonacci numbers
   */
  val fib: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] =
      cons(a, loop(b, a + 1))
    loop(0, 1)
  }

  /**
   * Exercise 5.11: Write a function called unfold. Taking an initial state and a function for producing both the
   * next state and the next value in the generated stream
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  /**
   * Exercise 5.12: Write fibs, from, constant and ones in terms of unfold
   */
  val onesUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

  /*
  I didn't get this...

    Scala provides shorter syntax when the first action of a function literal is to match on an expression.
    The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p => p match { case (f0,f1) => ... }`,
    but we avoid having to choose a name for `p`, only to pattern match on it.
   */
  val fibsUnfold: Stream[Int] = unfold((1, 1)){ case (a, b) => Some(b, (b, a+1)) }

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(_ => Some((n, n+1)))

  def constant[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))
}