package chapter14.st

/**
  * local effects monad ST (think state transition)
  */
sealed trait ST[S,A] { self =>
  protected def run(s: S): (A,S)
  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A) = {
    lazy val memo = a
    new ST[S,A] {
      def run(s: S) = (memo, s)
    }
  }
}

/**
  * methods to read/write on the cell are pure since they just return ST Actions. Note that the type of
  * ST isn't the type of the cell that's being mutated
  *
  * note STRef is sealed and the only way to
  */
sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S, A] = ST(cell)
  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    var cell = a
  })
}

object Example {
  for {
    r1 <- STRef[Nothing, Int](1)
    r2 <- STRef[Nothing, Int](1)
    x  <- r1.read
    y  <- r2.read
    _  <- r1.write(y + 1)
    _  <- r2.write(x + 1)
    a  <- r1.read
    b  <- r2.read
  } yield(a, b)
}
