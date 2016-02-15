package chapter12

import chapter11.Functor

/**
  * The applicative trait is less powerful than the monad one, but that comes with it's own benefits
  *
  * All applicatives are functors. implement map in terms of map2 and unit
  */
trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]
  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))  // note the unit(()) isn't a typo - () is sole value of type Unit

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a,b) => map2(f(a), b)(_ :: _))
}
