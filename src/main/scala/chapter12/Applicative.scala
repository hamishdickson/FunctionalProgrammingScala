package chapter12

import chapter11.Functor

import scala.language.higherKinds

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

  /**
    * Exercise 12.1: Trasplant the implementations of as many combinators as you can from Monad to Applicative,
    * using only map2 and unit or methods implemented in terms of them
    */
  //lma.foldRight(unit(List[A]()))((h,t) => map2(h, t)(_ :: _))
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))

  /**
    * Exercise 12.2
    * applicative comes from the fact we can formulate the Applicative interface using an alternative set of
    * primitives: unit and apply, rather than unit and map2. Define map and map2 in terms of unit and apply. Also show
    * the opposite
    */
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))
  def unit_byMaps[A](a: => A): F[A] =
    map(Unit[F[A]])(_ => a)

  def map_byApply[A,B](fa: F[A])(f: A => B): F[B] =
    apply[A,B](unit(f))(fa) // this needs the explicit type for some reason...
  def map2_byApply[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = apply[B,C](map(fa)(f.curried))(fb)

  /**
    * Exercise 12.3: Implement map3 and map4 using only unit, apply and the curried method available on functions
    *
    * nb: if `f: (A,B) => C` then `f.curried` has the type `A => B => C`
    */
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply[C,D](apply[B, C => D](apply[A, B => C => D](unit(f.curried))(fa))(fb))(fc)

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply[D,E](apply[C,D => E](apply[B, C => D => E](apply[A, B => C => D => E](unit(f.curried))(fa))(fb))(fc))(fd)
}
