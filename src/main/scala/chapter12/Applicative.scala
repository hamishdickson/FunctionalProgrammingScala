package chapter12

import java.util.Date

import chapter11.Functor

import scala.language.{reflectiveCalls, higherKinds}

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

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))
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