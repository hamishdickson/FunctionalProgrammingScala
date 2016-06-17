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

  def PrintLine(msg: String): OldIO = new OldIO {
    def run = println(msg)
  }

  // this is now pure - it returns an IO value. Say contest4 is effectful, but it's only PrintLine (the
  // interpreter) which has a side effect
  def contest4(p1: Player, p2: Player): OldIO = PrintLine(winnerMsg(winner(p1, p2)))
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

/**
  * This is actually trampolining - turns out you can modify any function type A => B to be A => TailRec[B] instead
  * Identical to the IO monad above, but used as a stack safe way to do things
  */
object TailRecMonad {
  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen(Return(_)))
  }

  case class Return[A](a: A) extends TailRec[A]
  // suspend means execute some effect to produce a result
  case class Suspend[A](resume: () => A) extends TailRec[A]
  // extend or continue
  case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
}


object AsyncMonad {
  import chapter7.Par._
  import chapter7.Par

  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)

    def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A] // notice, was a thunk in TailRec
  case class FlatMap[A,B](sub: Async[A], k: A => Async[B]) extends Async[B]

  // since we are no longer using () => A in Suspend, we now use a different tail recursive step function
  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)
    case Suspend(r) => Par.flatMap(r)(a => run(a))
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(a))
      case _ => sys.error("Impossible: `step` eliminates these cases")
    }
  }
}

/**
  * The difference between Free and TailRec is that free is parameterized with type constructor F
  */
sealed trait Free[F[_], A] {
  // exercise 13.1 implement map and flatmap
  def flatMap[B](f: A => Free[F,B]): Free[F,B] = FlatMap(this, f)
  def map[B](f: A => B): Free[F,B] = flatMap(f andThen(Return(_)))
}
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

object Typeys {
  import chapter7.Par
  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par,A]
}

object FreeStuff {
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = new Monad[({type f[a] = Free[F,a]})#f] {
    def unit[A](a: => A) = Return(a)
    def flatMap[A,B](fa: Free[F, A])(f: A => Free[F, B]) = fa flatMap f
  }

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(b) => b
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(b) => runTrampoline{ f(b) }
      case Suspend(r) => runTrampoline{ f(r()) }
      case FlatMap(a0,g) => runTrampoline { a0 flatMap { a0 => g(a0) flatMap f } }
    }
  }

  // generic interpreter for Free[F,A]
  def run[F[_], A](a: Free[F,A])(implicit F: Monad[F]): F[A] = a match {
    case Return(b) => F.Unit(b)
    case Suspend(r) => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _ => sys.error("Impossible, step eliminates these")
  }
}

object ConsoleMonad {
  import chapter7.Par

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: () => A

    def toReader: ConsoleReader[A]
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)
    def toThunk = () => run

    def run: Option[String] =
      try Some(readLine())
      catch { case e: Exception => None }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))
    def toThunk: () => println(line)
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
  }

  val f1: Free[Console, Option[String]] = for {
    _ <- printLn("I can only interact with the console")
    ln <- readLn
  } yield ln

  /**
    * to actually run this, we need to translate our Console type
    */
  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): F[A]
  }

  type ~>[F[_], G[_]] = Translate[F,G]

  val consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]) = a.toThunk
  }

  val consoleToPar = new (Console ~> Par) {
    def apply[A](a: Console[A]) = a.toPar
  }

  def runFree[F[_], G[_], A](free: Free[F,A])(f: F ~> G)(implicit G: Monad[G]): G[A] = step(free) match {
    case Return(a) => G.unit(a)
    case Suspend(r) => t(r)
    case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
    case _ => sys.error("not possible")
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A = runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] = runFree[Console,Par,A](a)(consoleToPar)

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A,B](a: Function0[A])(f: A => Function0[B]) =
      () => f(a())()
  }

  implicit val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A,B](a: Par[A])(f: A => Par[B]) =
      Par.fork { Par.flatMap(a)(f) }
  }

  /**
    * Exercise 13.4: flatmap for function0 isn't stack safe, implement translate using runFree and the use it to
    *  implement runConsole in a stack safe way
    */
  def translate[F[_], G[_], A](f: Free[F,A])(fg: F ~> G): Free[G,A] = {
    type FreeG[A] = Free[G,A]
    val t = new (F ~> G) {
      apply[A](a: F[A]): Free[G,A] = Suspend { fg(a) }
    }
    runFree(f)(t)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console,A]): A =
    runTrampoline { translate(a)(new (Console ~> Function0) {
      def apply[A](c: Console[A]) = c.toThunk
    })}

  // could have an interpreter which does nothing or always returns the same value (ie for a test)
  // String => A
  case class ConsoleReader[A](run: String => A) {
    def map[B](f: A => B): ConsoleReader[B] = ConsoleReader(r => f(run(r)))
    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)).run(r))
  }

  object ConsoleReader {
    implicit val monad = new Monad[ConsoleReader] {
      def unit[A](a: => A) = ConsoleReader(_ => a)
      def flatMap[A,B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) = ra flatMap f
    }
  }

  val consoleToReader = new (Console ~> ConsoleReader) {
    def apply[A](a: Console[A]) = a.toReader
  }

  @annotation.tailrec
  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
    runFree[Console,ConsoleReader,A](io)(consoleToReader)

  
}

abstract class App {
  import java.util.concurrent._

  import ConsoleMonad._
  import chapter7.Par
  import chapter7.Par._
  /**
    * intercepts the IO action and actually performs the effect
    */
  def unsafePerformIO[A](a: IO[A])(pool: ExecutorService): A =
    Par.run(pool)(run(a)(parMonad))

  def main(args: Array[String]): Unit = {
    val pool = Executors.fixedThreadPool(8)
    unsafePerformIO(pureMain(args))(poo)
  }

  def pureMain(args: IndexedSeq[String]): IO[Unit]
}
