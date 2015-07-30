package chapter1

import scala.annotation.tailrec

/**
 * Exercise 2.1: Write a recursive function to get the nth Fibonacci number. Your definition should use a local
 * tail recursive function
 */

object Fibonacci {
  def main(args: Array[String]): Unit = {
    println(fibonacci(1)) // 1
    println(fibonacci(2)) // 1
    println(fibonacci(3)) // 2
    println(fibonacci(4)) // 3
    println(fibonacci(5)) // 5
    println(fibonacci(6)) // 8
  }
  
  def fibonacci(n: Int): Int = {
    @tailrec
    def go(n: Int, a: Int, b: Int): Int =
      if (n <= 0) a
      else go(n - 1, b, a + b)
    go(n, 0, 1)
  }
}