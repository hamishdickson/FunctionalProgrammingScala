package chapter4

import chapter3.Cons

import scala.annotation.tailrec
import scala.util.Try

sealed trait Option[+A] {

  /**
   * Change each element in the option if it exists
   */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  // apply f, which may fail to the Option if not None
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  // here, B must be a super type of A
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  /**
   * hard
   *
   * Exercise 4.2: Implement variance in terms of flatMap. if the mean of a seq is m, the variance is the mean of
   * math.pow(x - m, 2)
   */
  def variance(xs: Seq[Double]): Option[Double] = {

    // first you need to work out the mean
    def mean(ys: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.size)
    }

    // get the mean, then flatMap it
    // need the mean for error handling (I think)
    // map the "elements" (there should be only one) to the variance
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  /**
   * lift returns a function which maps None to None and applies f to the contents of Some. f need not be aware
   * of the Option type at all
   */
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  /**
   * hard to work out - good question!!
   *
   * Exercise 4.3: Write a generic function map2 that combines two Option values using a binary function. If either
   * Option value is None, then the return value is too.
   *
   * Similar logic for map3, map4...
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap(aa => b map (bb => f(aa, bb)))

  /**
   * as a for...
   *
   * for-comprehensions in scala consist of a seq of bindings like aa <- a followed by a yield
   *
   * the compilier de-sugars this to a flatmap, with the final binding and yield converted to a map
   */
  def map2_for[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  /**
   * Exercise 4.4: Write a function sequence that combines a list of Options into one Option containing a list
   * of all the same Some values in the original list. If the original list contains None even once, the result
   * of the function should be None; otherwise the result should be Some with a list of all the values
   *
   * Annoyingly, you have to tell foldRight of the type here, otherwise it incorrectly thinks Some(Nil) is something
   * rubbish
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a, b)(_ :: _))

  // you can also do it recursively apparently
  def sequence_rec[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap(hh => sequence_rec(t) map(hh :: _))
  }

  /**
   * Exercise 4.5: Implement traverse
   *
   * Try and do this so that it only looks at the list once
   */
  def traverse_slow[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a map (i => f(i)))

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

}





