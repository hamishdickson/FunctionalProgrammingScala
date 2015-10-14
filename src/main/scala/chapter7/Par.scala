package chapter7

trait Par[+A]

object Par {
  // creates a computation that immediately results in the value a
  def unit[A](a: A): Par[A] = ???
  // computes the result of 2 parallel computations with a binary function
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = ???
  // marks a computation for concurrent evaluation by run
  def fork[A](a: => Par[A]): Par[A] = ???
  // wraps the expression a for concurrent evaluation by run
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  // fully evaluates a given Par, spawning parallel computations as requested by for and extracting the resulting value
  def run[A](a: Par[A]): A = ???
}
