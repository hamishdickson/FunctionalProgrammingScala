package chapter11

import scala.language.higherKinds

/**
  * type constructors: remember a type constructor is applied to a type to produce a type, eg List is a type constructor
  * not a type
  */
trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  /**
    * Distribute, this is sometime also called `unzip`
    */
  def distribute[A,B](fab: F[(A,B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  /**
    * When we have an operation on a product like this, we should see if we can construct the opposite operation over a
    * sum or coproduct
    */
  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A,B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  /**
    * we say that a type constructor like list or option or F is a functor and the Functor[F] instance constitues
    * proof that F is in fact a functor
    */
  val listFunction = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }
}