package chapter7

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

/**
 * A note on Future - Future isn't functional in itself, but we still are purely functional - we are keeping our
 * API functional - see part 4 for more details
 */

trait Par[+A]

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /**
   * creates a computation that immediately results in the value a
   *
   * `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future`
   * that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be
   * cancelled. Its `get` method simply returns the value that we gave it.
   */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // computes the result of 2 parallel computations with a binary function
  /**
   * `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having
   * `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))`
   * if we want the evaluation of `f` to occur in a separate thread.
   *
   * Note (and exercise 7.3): This implementation of `map2` does _not_ respect timeouts. It simply passes the
   * `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them,
   * and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that
   * records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for
   * evaluating `bf`.
   */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  // marks a computation for concurrent evaluation by run
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  // wraps the expression a for concurrent evaluation by run
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  // fully evaluates a given Par, spawning parallel computations as requested by for and extracting the resulting value
  def run[A](a: Par[A]): A = ???

  /**
   * Exercise 7.4: This API already enables  a rich set of operations. Here's a simple example: using lazyUnit, write a function to
   * convert any function A => B to one that evaluates it's result async
   */
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  /**
   * So this looks like a massive hack - I'd like to sort my list of ints, but I don't really have a function to apply
   * in map2 - so pass a thunk type thing.. it looks like it should work though
   */
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  /**
   * using the idea behind sortPar, we can lift any function A => B to be Par[A] => Par[B]
   */
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar2(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  /**
   * Can we map over a list in parallel? This would need to combine N parallel computations
   */
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /**
   * Exercise 7.5: Write sequence - needs to convert List[Par[A]] to Par[List[A]]
   *
   * I nearly got there with this - I didn't get the unit part though (just tried List()) - which was silly, it wouldn't
   * have the right type without it
   *
   * Note: you can find better implementations here: https://github.com/fpinscala/fpinscala/blob/8440f61e54dcd60087d5fdd6a0d3fb73934abed0/answers/src/main/scala/fpinscala/parallelism/Par.scala
   */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((a, b) => map2(a, b)(_ :: _))
}
