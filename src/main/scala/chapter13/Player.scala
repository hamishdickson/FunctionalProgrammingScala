package chapter13

import scala.language.higherKinds

case class Player(name: String, score: Int)

object Game {
  def contest(p1: Player, p2: Player): Unit = {
    if (p1.score > p2.score)
      println(s"${p1.name} is the winner!")
    else if (p2.score > p1.score)
      println(s"${p2.name} is the winner!")
    else
      println("It's a draw!")
  }

  /**
    * the problem with contest is it mixes computation with io
    * you can ALWAYS refactor the computation out into a pure program
    */
  def winner(p1: Player, p2: Player): Option[Player] = {
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None
  }

  def impureContest(p1: Player, p2: Player): Unit = winner(p1, p2) match {
    case Some(Player(name, _)) => println(s"$name won!")
    case None => println("its a draw")
  }

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner"
  } getOrElse "It's a draw"

  // note this is the outermost part of the program - the only one with side effects
  def contest3(p1: Player, p2: Player): Unit = println(winnerMsg(winner(p1, p2)))

  /**
    * given an impure function of type f: A => B, we can split f into
    *  - a pure function of type A => D where D is some description of the result of f
    *  - an impure function of type D => B which can be thought of as the interpreter of these descriptions
    */

  def PrintLine(msg: String): IO = new IO {
    def run = println(msg)
  }

  // this is now pure - it returns an IO value. Say contest4 is effectful, but it's only PrintLine (the
  // interpreter) which has a side effect
  def contest4(p1: Player, p2: Player): IO = PrintLine(winnerMsg(winner(p1, p2)))
}


/**
  * on it's own this isn't that intersting, in fact it's just a monoid (run == zero, ++ == associative binaray op)
  *  only really lets us delay when side effects actually happen
  */
trait OldIO { self =>
  def run: Unit
  def ++(io: OldIO): OldIO = new OldIO {
    def run = {
      self.run
      io.run
    }
  }
}

object OldIO {
  def empty: OldIO = new OldIO {
    def run = ()
  }
}


// OK lets create something that can also handle input

sealed trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] = new IO [B] {
    def run = f(self.run)
  }

  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    def run = f(self.run).run
  }
}

// extends applicative really
trait Monad[F[_]] {
  def Unit[A](a: => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

// cool .. map.. flatmap .. a unity thing => monad
object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] {
    def run = a
  }

  def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)
}


object Converter {
  def ReadLine: IO[String] = IO { readLine }

  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()
}

/**
  * this io monad is useful for programs that keep running. normally you'd run into stackoverflow
  *  problems. Instead, this works by baking in the flow we want to support
  * 
  * Instead of making flatMap a method that consturcts a new IO in terms of run, we can just make it
  * a data consturctor of the IO data type.
  */
object IOa2 {
  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def map[B](f: A => B): IO[B] = flatMap(f andThen(Return(_)))
  }

  // 3 different types of control flow we want the interpreter of this data type to support
  // return => finished
  case class Return[A](a: A) extends IO[A]
  // suspend means execute some effect to produce a result
  case class Suspend[A](resume: () => A) extends IO[A]
  // extend or continue
  case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

  def printLine(s: String): IO[Unit] = Suspend(() => Return(println(s)))

  // tailrec run interpreter
  @annotation.tailrec def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r))
      case FlatMap(y, g) => run(y flatMap(a => g(a) flatMap(f)))
    }
  }
}
