package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
   * Exercise 2.25: Write a function size that counts the number of nodes (leaves and branches) in a tree
   */
  def size[A](as: Tree[A]): Int = as match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }
}