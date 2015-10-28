package chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object Nonblocking {
  sealed trait Future[A] {
    private[Parallelism] def apply(k: A => Unit): Unit
  }

  // this looks the same, but with a non-blocking future
  type Par[+A] = ExecutorService => Future[A]

  object Par {
    /**
     * Note, it's not actually possible to write a non-blocking version of run - the simple reason is you have to actually
     * get a thread
     *
     * We don't want users to call this unless they actually want it
     */
    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
      latch.await // Block until the `latch.countDown` is invoked asynchronously
      ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
    }

    // so what would unit look like?
    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })
  }
}