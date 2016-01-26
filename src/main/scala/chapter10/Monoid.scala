package chapter10

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
}