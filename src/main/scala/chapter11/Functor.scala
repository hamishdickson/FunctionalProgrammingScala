package chapter11

import scala.language.higherKinds

/**
  * type constructors: remember a type constructor is applied to a type to produce a type, eg List is a type constructor
  * not a type
  */
trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  val listFunction = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }
}
