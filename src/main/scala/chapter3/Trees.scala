package chaspter3

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
}