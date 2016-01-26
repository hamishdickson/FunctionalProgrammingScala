package chapter10

/**
  * A monoid consists of the following:
  * - some type A
  * - an associative binary operation `op`, that takes values of type A and combines them into another
  *       op(op(x,y),z) == op(x, op(y,z)) ∀ x,y,z in A
  * - a value `zero` that is the identity for `op`, ie op(x, zero) == x and op(zero, x) == x
  */

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}
