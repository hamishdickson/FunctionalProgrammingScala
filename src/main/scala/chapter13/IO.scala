package chapter13

import scala.language.higherKinds

// in reality this extends applicative
trait MonadMcMonadface[F[_]] {
  def Unit[A](a: => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

