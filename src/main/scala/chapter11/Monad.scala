package chapter11

import chapter4.{Option, Some}
import chapter6.State

import scala.language.higherKinds

/**
  * Monad
  *
  * Remember map is actually composed of unit - so make that a primitive. ie to use this we only need to implement
  * unit and flatMap, then we get map and map2 for nothing :)
  *
  * note, monad is a functor
  *
  *
  * weirdly, there are 3 (seemingly equivalent) definitions of monad:
  * 1. unit, map, flatmap
  * 2. unit, compose and map
  * 3. map, unit and join
  *
  * along with the laws for associativity and identity
  *
  *
  * Monads: provide a context for introducing and binding variables and performing substitution
  */
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))

  /////////////////////////////////////

  /**
    * Exercise 11.3: Implement sequence and traverse
    */
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((h,t) => map2(h, t)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((h,t) => map2(f(h), t)(_ :: _))

  /**
    * Exercise 11.4: Replicate M (times)
    *
    * create a list n long of ma and use sequance over it
    */
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def product[A,B](ma: F[A], mb: F[B]): F[(A,B)] = map2(ma, mb)((_, _))

  /**
    * Exercise 11.6: Implement filterM
    */

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]()))((x,y) => compose(f, (b: Boolean) => if (b) map2(unit(x),y)(_ :: _) else y)(x))


  /**
    * Kleisli
    *
    * Exercise 11.7: Implement the kleisli composition function compose
    */
  def composeKleisli[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  /**
    * Exercise 11.8: Implement flatMap in terms of unit and compose
    *
    * I actually got this using basically dimensional analysis, but couldn't work out the Unit bit
    */
  def flatMap2[A,B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)(())

  /**
    * Identity laws:
    *
    * Left and right laws:
    * compose(f, unit) == f
    * compose(unit, f) == f
    *
    * or in terms of flatMap
    * flatMap(x)(unit) == x
    * flatMap(unit(y))(f) == f(y)
    */

  /**
    * Exercise 11.12 implement join in terms of flatmap
    */
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(i => i)

  /**
    * Exercise 11.13: Implement flatmap in terms of join and map
    */
  def flatMap3[A,B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
}

object Monad {
  /**
    * Exercise 11.1: Write monad instances for Option, Stream and List
    */
  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa flatMap f
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] = fa flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa flatMap f
  }

  /**
    * Exercise 11.2: State looks like it would be a monad too, but it takes 2 type args and you need a type
    * constructor of one arg to implement Monad. Try to implement State monad
    */
  //type State[S,A] = (S,A) // nope

  class StateMonads[S] {
    type StateS[A] = State[S, A]

    // We can then declare the monad for the `StateS` type constructor:
    val monad = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }
  }

  /**
    * See below - this is actually really fucking cool
    */
  // But we don't have to create a full class like `StateMonads`. We can create
  // an anonymous class inline, inside parentheses, and project out its type member,
  // `lambda`:
  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  /**
    * Example, getting and setting state with a for-comprehension
    */
  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] = as.foldLeft(F.unit(List[(Int,A)]()))((acc, a) => for {
    xs <- acc
    n <- getState
    _ <- setState(n + 1)
  } yield (n,a) :: xs).run(0)._1.reverse

  def getState[S]: State[S,S] = State(i => (i,i))
  def setState[S](s: S): State[S,Unit] = State(_ => ((), s))


  val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = fa.flatMap(f)

    override def unit[A](a: => A): Id[A] = Id(a)
  }

}

/**
  * Exercise 11.17: Implement map and flatmap as methods on this class and give an implementation for Monad[Id]
  */
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

/*
This thing:

object IntStateMonad extends Monad[((type IntState[A] = State[Int,A]))#IntState] {

}

is a Monad with an anonymous type, which is kind of cool. You access the type with #, this is called type projection and
is a bit like using a . to access a member at value-level programming

a type constructor declared inline like this is called a `type lambda` in scala. we can use this trick to partially
apply the State type constructor and declare the State-Monad trait
*/
