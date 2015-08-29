package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
   * Exercise 2.25: Write a function size that counts the number of nodes (leaves and branches) in a tree
   */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  /**
   * Exercise 2.26: Implement max, a function for Int Trees to find the max element in a tree
   */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /**
   * Exercise 2.27: Write depth, which returns the depth of the deepest leaf
   */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  /**
   * Exercise 2.28: Write a function map, analogus to map in List
   */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
   * Exercise 2.29: Write a function fold, then use it for these other
   */
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(n) => f(n)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximum2(t: Tree[Int]): Int = fold(t)(n => n)(_ max _)

  def depth2[A](t: Tree[A]): Int = fold(t)(a => 0)((l, r) => 1 + (l max r))

  /**
   * Needed help with the types here, apparently it's a scala thing - see solutions
   */
  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(n => Leaf(f(n)): Tree[B])(Branch(_, _))
}