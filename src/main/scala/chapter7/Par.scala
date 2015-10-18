package chapter7

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

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
}
