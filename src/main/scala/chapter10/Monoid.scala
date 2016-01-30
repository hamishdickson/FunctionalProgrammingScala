package chapter10

import org.scalacheck.{Prop, Gen}

/**
  * A monoid consists of the following:
  * - some type A
  * - an associative binary operation `op`, that takes values of type A and combines them into another
  *       op(op(x,y),z) == op(x, op(y,z)) ∀ x,y,z in A
  * - a value `zero` that is the identity for `op`, ie op(x, zero) == x and op(zero, x) == x
  *
  *
  * Useful to know:
  * - means you can parallelise problem
  * - can be composed to assemble complex calculations from simpler pieces
  */

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  /**
    * Example - string monoid
    */
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  /**
    * List concatenation also forms a monoid
    */
  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  /**
    * Exercise 10.1: Give instances for the below
    */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  /**
    * Exercise 10.2: Give monoid an instance for combining `Option` values
    *
    * note: could do this the other way around - this is known as the `dual`
    */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  /**
    * Exercise 10.3: A function having the same argument and return type is called an `endofunction`, write a monoid
    * for endofunctions
    *
    * endo == within => endofunctions codomain is in it's domain
    */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2

    override def zero: (A) => A = (a: A) => a
  }

  /**
    * Exercise 10.4: use property based testing to create a laws tester
    */
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(for {
      i <- gen
      j <- gen
      k <- gen
    } yield (i,j,k)){ case (a,b,c) => m.op(a, m.op(b, c)) == m.op(m.op(a, b), c) } &&
    Prop.forAll(gen)(a => m.op(a, m.zero) == a && m.op(m.zero, a) == a)


  /**
    * In general, foldRight and foldLeft won't form a monoid with a list, but instead of
    *
    * foldRight[B](z:B)(f: (A,B) => B): B
    *
    * imagine if A and B were the same:
    *
    * foldRight[A](z: A)(f: (A,A) => A): A
    *
    * This looks a lot like zero and op from a monoid
    *
    * We can create a function `concatenate` which folds a list with a monoid
    */
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /**
    * but what if our list has an element type that doesn't have a monoid instance? Well, we can always map
    *
    * Exercise 10.5: Implement this
    */
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldRight(m.zero)((a,b) => m.op(f(a), b))

  /**
    * Exercise 10.6: write foldLeft and foldRight in terms of foldMap
    */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(f.curried)(z)

  /**
    * A monoid is useful when thinking about parallelism. Consider how you would foldRight a,b,c,d
    *
    * op(a, op(b, op(c, d)))
    *
    * a balanced fold would look more like this
    *
    * op(op(a,b), op(c,d))
    *
    * Exercise 10.7: Implement foldMap for IndexedSeq (supports splitAt and length). Your implementation should use
    * the strategy of splitting the sequence in two, recursively processing each half and then adding the answers
    * together with the monoid
    */
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.isEmpty) m.zero
    else {
      if (v.length == 1) f(v.head)
      else {
        val (l,r) = v.splitAt(v.length / 2)
        m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
      }
    }

  /**
    * Exercise 10.9: Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You'll need to come up with
    * a creative monoid
    *
    *
    * OK - so this is kinda cool
    */
  val orderedMonoid = new Monoid[Option[(Int, Int, Boolean)]] {
    def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]) =
      (a1, a2) match {
        case (Some((x1, y1, p)), Some((x2, y2, q))) => Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
        case (x, None) => x
        case (None, x) => x
      }
    val zero = None
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = foldMapV(ints, orderedMonoid)(i => Some((i, i, true))).map(_._3).getOrElse(true)


  /**
    * Example: Word counting
    *
    * Imagine this big book
    */
  val bigBook: String = "lorem ipsum dolor sit amet, "

  /**
    * If we wanted to count the words in parallel, we would split this in 2 and that.. but we'd probably split some
    * words in 2 (resulting in double counting)
    *
    * Partial result of a word count could be represented by an algebraic data type
    */
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Parts(lStub: String, words: Int, rStub: String) extends WC

  val monoidWC: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Parts(i, j, k)) => Parts(a + i, j, k)
      case (Parts(i, j, k), Stub(a)) => Parts(i, j, k + a)
      case (Parts(a,b,c), Parts(i,j,k)) => Parts(a, b + (if ((c + i).isEmpty) 0 else 1) + j, k)
    }

    override def zero: WC = Stub("")
  }
}