package chapter2

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n*acc)

    go(n, 1)
  }

  /**
   * notice formatAbs and formatFactorial are basically the same
   */
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  /**
   * Write a higher order function to take their place
   */
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  /**
   * example of a monomorphic function
   *
   * gives first index of a key in an array (or -1 if not found)
   */
  private def findFirst(ss: Array[String], key: String): Int = {
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  /**
   * example of parametric polymorphic function (aka generic function)
   *
   * idea here is that the findFirst logic is the same no matter what type of array we are
   * looking at - so should be able to do this with any type of array
   *
   * instead of hard coding a string, take a type A as a parameter and instead of hard coding
   * equality of the key, take a function to test for equality
   */
  private def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n // if p gives true, then found the key
      else loop(n + 1)

    loop(0)
  }

  /**
   * Exercise 2.2: Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison
   * function
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int, m: Int): Boolean = {
      if (m >= as.length) true
      else if (!ordered(as(n), as(m))) false
      else loop(m, m+1)
    }
    loop(0, 1)
  }

  /**
   * partial applictaion: takes a value and a function of two args and returns a function of one arg as the
   * result
   */
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  /**
   * Example 2.3: Currying. Converts a function f of two args into a function of one arg that partially applies f
   * There is only one implementation that compiles - write it
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))

    println(formatResult("factorial", 7, factorial))

    // note you can do things with an anoymous function
    findFirst(Array(7, 9, 13), (x: Int) => x == 9)
  }
}
