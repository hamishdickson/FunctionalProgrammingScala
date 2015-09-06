package chapter4

/**
 * Either represents a situation that can have one of 2 values.
 *
 * Say it's a disjoint union of 2 types
 *
 * When we use it for success/failure - by convention use Right for success, Left for failure
 */

sealed trait Either[+E, +A] {
  /**
   * Exercise 4.6: Implement all this stuff
   */
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  // do the right thing or b
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(e) => b
  }

  // must learn these for comprehensions
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a,bb)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  // can return a different object/string
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  // can throw an exception if we want
  def saveDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x/y)
    catch { case e: Exception => Left(e) }

  // can create our own Try function
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  // note we can do this
  def parseInsuranceRateQuote(age: String, numberOfTickets: String): Either[Exception, Double] =
    for {
      a <- Try(age.toInt)
      b <- Try(numberOfTickets.toInt)
    } yield quote(a, b)

  def quote(a: Int, b: Int): Double = 5.0
}