package chapter10

import org.scalacheck.{Prop, Gen}

import scala.language.higherKinds

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
  *
  * Kind of useful, but the cool fact about monoids is they `compose`. In this context it means if A and B are monoids
  * then the tuple type (A,B) is also a monoid (called their `product`)
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

  /**
    * Exercise 10.10: Write a monoid for WC
    */
  val monoidWC: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Parts(i, j, k)) => Parts(a + i, j, k)
      case (Parts(i, j, k), Stub(a)) => Parts(i, j, k + a)
      case (Parts(a,b,c), Parts(i,j,k)) => Parts(a, b + (if ((c + i).isEmpty) 0 else 1) + j, k)
    }

    override def zero: WC = Stub("")
  }

  /**
    * Exercise 10.11: Use the WC monoid to implement a function that counts words in a string by recursively splitting
    * it into substrings and counting the words in those substrings
    */
  def countWords(s: String): Int = {
    // A single character's count. Whitespace does not count,
    // and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Parts("", 0, "")
      else
        Stub(c.toString)
    // `unstub(s)` is 0 if `s` is empty, otherwise 1.
    def unstub(s: String) = s.length min 1
    foldMapV(s.toIndexedSeq, monoidWC)(wc) match {
      case Stub(s) => unstub(s)
      case Parts(l, w, r) => unstub(l) + w + unstub(r)
    }
  }


  /**
    * Exercise 10.16: prove that if A and B are monoids, then their product (A,B) is also a monoid
    */
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

    override def zero: (A, B) = (A.zero, B.zero)
  }

  /**
    * Exercise 10.17: Write a monoid instance for functions whose results are monoids
    */
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    override def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1(a), a2(a))

    override def zero: (A) => B = a => B.zero
  }

  /**
    * Exercise 10.18: A bag is like a set, except that it's represented by a map that contains one entry per element
    * with that element as the key and the value under that key is the number of times the element appears in tha bag
    */
  def manMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K,V]] =
    new Monoid[Map[K, V]] {
      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a1.getOrElse(k, V.zero),
                            a2.getOrElse(k, V.zero)))
      }

      override def zero: Map[K, V] = Map[K,V]()
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = foldMapV(as, manMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))
}

/**
  * In general, when we want to fold something we don't really care about what the thing is (a list, a stream...)
  *
  * Here, abstract over F[_], where the _ indicates that F is not a type, but a type-constructor that takes one
  * argument. Just like functions that take other functions as arguments are called higher-order functions, Foldable
  * is a `higher-order type constructor` or `higher-kinded type`
  *
  * Exercise 10.15: Any foldable can be turned into a list, implement this
  */
trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())(_ :: _)
}

/**
  * Exercise 10.12: Implement Foldable[List], Foldable[IndexedSeq] and Foldable[Stream]
  */
object ListFoldable extends Foldable[List] {
  // by definition
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  // by definition
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B =
    as.foldRight(mb.zero)((a,b) => mb.op(f(a), b))

  override def toList[A](fa: List[A]): List[A] = fa
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((a,b) => mb.op(f(a), b))
}

/**
  * Exercise 10.13: Implement Foldable for a binary tree
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => mb.op(f(a), mb.zero)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
}

/**
  * Exercise 10.14: Implement foldable for option
  */
object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(a) => f(a, z)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(a) => f(z, a)
  }

  override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }
}