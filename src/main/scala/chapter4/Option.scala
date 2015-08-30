package chapter4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  // apply f, which may fail to thw Option if not None
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  // here, B must be a supertype of A
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  /**
   * Exercise 4.2: Implement variance in terms of flatmap. if the mean of a seq is m, the variance is the mean of
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
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
