package chapter4

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
   * Exercise 4.3: Write a generic function map2 that combines two Option values using a binary function. If either
   * Option value is None, then the return value is too.
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))
}
