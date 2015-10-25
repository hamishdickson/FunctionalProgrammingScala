package chapter7

import java.util.concurrent.ExecutorService

object Nonblocking {
  sealed trait Future[A] {
    private[Parallelism] def apply(k: A => Unit): Unit
  }

  // this looks the same, but with a non-blocking future
  type Par[+A] = ExecutorService => Future[A]
}
