package chapter12

import java.util.Date

import chapter10.{Foldable, Monoid}
import chapter11.Functor

import scala.language.{higherKinds, reflectiveCalls}

/**
  * The applicative trait is less powerful than the monad one, but that comes with it's own benefits
  *
  * All applicatives are functors. implement map in terms of map2 and unit
  *
  * Note: All Monads are applicative Functors
  *
  * Laws:
  * 1) left and right identity:
  *   First off, we expect applicatives to satisfy the Functor laws:
  *   - map(v)(id) == v
  *   - map(map(v(g))(f) == map(v)(f compose g)
  *
  *   here, we've implemented map in terms of map2, as `map2(fa, unit(()))((a, _) => f(a))`. This was abitary and could
  *   have been `map2(unit(()), fa)((_, a) => f(a))` these are the left and right identity laws.
  * 2) Associativity:
  *   recall we wrote map3 in terms of apply and unit. Instead we could have used map2. This would involve using map2
  *   with 2 or the args, then their result on the 3rd. There are 2 ways to do this -> associative.
  *
  * 3) Naturality of product
  *   Doesn't matter if you apply a transform before or after map2 (say), eg
  *   `map2(a,b)(productF(f,g)) == product(map(a)(f), map(b)(g))`
  *
  *   where productF combines two functions into one function that takes both as args and returns the pair of their
  *   results:
  *
  *   def productF[I,O,I2,O2](f: I => O, g: I2 => O2): (I,I2) => (O,O2) =
  *     (i,i2) => (f(i), g(i2))
  *
  *
  */
trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = apply(map(fa)(f.curried))(fb)
  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A,B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))  // note the unit(()) isn't a typo - () is sole value of type Unit

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
    * Exercise 12.12: So if we look at sequence, it's a bit weird that we have a concrete type (List) in an abstract interface
    * maybe we can abstract over it?
    *
    * Implement sequence over Map rather than a List
    *
    * Notes: Tried this with foldRight for ages before trying foldLeft and it just working - not sure what that's about
    * (commutivity?)
    */
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldLeft(unit(Map.empty[K,V])){ case (a,(k,fv)) => map2(a, fv)((m, v) => m + (k -> v)) }

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

  /**
    * Exercise 12.8: Just as we can take the product of two monoids, A,B to give the monoid (A,B), we can take the
    * product of Applicatives - implement this
    */
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))

      override def map[A, B](fa: (F[A], G[A]))(f: (A) => B): (F[B], G[B]) = ???
    }
  }

  /**
    * Exercise 12.9: Applicative functors also compose another way - if F[_] and G[_] are applicative
    * functors, then so is F[G[_]]
    *
    * Hard - guessed unit, but not map2
    */
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C) = self.map2(fga, fgb)(G.map2(_,_)(f))

      override def map[A, B](fa: F[G[A]])(f: (A) => B): F[G[B]] = ???
    }
  }
}

object Applicative {

  /**
    * for streams we can define unit and map2, but not flatmap (ie a monad)
    */
  val streamApplicative = new Applicative[Stream] {
    override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] =
      fa zip fb map f.tupled

    override def unit[A](a: => A): Stream[A] = Stream.continually(a)

    /**
      * Exercise 12.4: What is the meaning of streamApplicative.sequence?
      */
    override def sequence[A](a: List[Stream[A]]): Stream[List[A]] = ???
  }

  /**
    * Exercise 12.6: write an applicative instance for Validation that accumulates errors in Failure. Note that
    * in the case of Failure there's always at least one error, stored in head. The rest of the errors accumulate
    * in tail
    */
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    // primitive combinators
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a,b))
        case (Failure(h1, t1), Failure(h2, t2)) =>
          Failure(h1, t1 ++ Vector(h2) ++ t2)
        //case (Failure(h, t), Success(a)) => Failure(h, t)
        //case (Success(a), Failure(h, t)) => Failure(h, t)
        case (e@Failure(_, _), _) => e // ??!
        case (_, e@Failure(_,_)) => e  // ??!
      }

    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }
}

/**
  * A minimal implementation of monad must implement `unit` and override either `flatMap` or `join` and `map`
  */
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa) = flatMap(ffa)(fa => fa)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))

  override def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))

  /**
    * Exercise 12.11: Try to write compose on Monad. It's not possible, but it's instructive to attempt it and
    * understand why this is the case
    *
    * http://tonymorris.github.io/blog/posts/monads-do-not-compose/
    */
  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = ???
}

object Monad {
  /**
    * Exercise 12.5: Write a monad instance for Either
    */
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A,B](eea: Either[E, A])(f: A => Either[E, B]) = eea match {
      case Right(a) => f(a)
      case Left(b) => Left(b) // notice it does nothing in the case of left
    }
  }
}

/**
  * Validation - a bit like Either, but it can explicitly handle more than one error
  */
sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

case class WebForm(name: String, birthdate: Date, phoneNumber: String) {
  def validName(name: String): Validation[String,String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String,Date] =
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch {
      case _: Throwable => Failure("Birthdate must be of form yy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9](10)"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  /**
    * Great, now we have those, we can lift the entire WebForm constructor with map3
    */
  /*def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
    map3(validName(name), validBirthdate(birthdate), validPhone(phoneNumber))*/
}

/**
  * Lets wrap up our generalisation of sequence over List and Map
  *
  *
  * So what is this? Well lets see what it does:
  * * List[Option[A]] => Option[List[A]] gives None if any of the input list is None
  * * Tree[Option[A]] => Option[Tree[A]] gives None if any of the input tree is None
  * * Map[K, Par[A]] => Par[Map[K,A]] produces a parallel computation that evaluates any all
  *      values of the map in parallel
  *
  * Traverse is similar to fold in that it takes some data structure and a function, but traverse preserves the
  * original structure
  *
  * Traverse is more general than `map`, it can also express `foldMap` (and so `foldLeft` and `foldRight`
  *
  *
  */
trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  /**
    * This does something quiet cool - if G is an applicative functor, then sequence swaps F and G in F[G[A]]
    */
  def sequence[G[_]: Applicative,A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)


  /**
    * Exercise 12.14: (hard) implement map in terms of traverse as a method on traverse[F]
    *
    * this implies that Traverse is an extension of Functor and that the traverse function is a
    * generalization of map (for this reason we sometimes call these traversable functors)
    *
    *
    * So nearly got this - realised you needed to specify what's going on in the type sig... didn't
    * realise you needed the idMonad though..
    */
  def map[A,B](fa: F[A])(f: A => B): F[B] = traverse[Id,A,B](fa)(f)(idMonad)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  /**
    * You can turn any monoid into an applicative provided there's a constant functor (see below for what that is)
    *
    * This means Traverse can extend Foldable and we give a default implementation of foldmap in terms of traverse
    */
  type Const[M,B] = B

  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({ type f[x] = Const[M,x] })#f] {
    override def unit[A](a: => A): M = M.zero
    def map2[A,B,C](m1: M, m2: M)(f: (A,B) => C): M = M.op(m1, m2)
  }

  def foldMap[A,M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({ type f[x] = Const[M, x]} )#f, A, Nothing](as)(f)(monoidApplicative(mb))
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  /**
    * Exercise 12.13: write traverse instances for List, Option and Tree
    */
  val listTraverse: Traverse[List] = new Traverse[List] {
    // I've done something wrong here with my definitions :,(
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f

    /**
      * OK .. so, took me a while to see that I needed G.unit here - then realised (cheated) you can use a typeclass
      * to provide the applicative - penny drop moment
      */
    def traverse[G[_]: Applicative,A,B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a,b) => G.map2(f(a), b)(_ :: _))
  }


  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa map f

    def traverse[G[_]: Applicative,A,B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = fa match {
      case Some(a) => G.map(f(a))(Some(_))
      case None => G.unit(None)
    }
  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = ???

    // OK the trick here is to see that you need listTraverse
    def traverse[G[_]: Applicative,A,B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(fa.head), listTraverse.traverse(fa.tail)(a => traverse(a)(f)))(Tree(_, _))
  }
}