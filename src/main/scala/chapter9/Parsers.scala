package chapter9

import org.scalacheck._

/**
  * Let's create a Parser!
  *
  * note: the weirdness going on with Parser[+_] - it means a type parameter for something that is itself a type
  * constructor
  *
  * note: the `self =>` thingy - this lets us use self later on, like in ParserOps
  */
trait Parsers[ParseError, Parser[+_]] { self =>
  /**
    * Simpliest parser we can imagine - it reads a char and returns this Parser thing
    *
    * Parser returns a type on success and information about the failure if there is one
    *
    * should satisfy `run(char(c))(c.toString) == Right(c)`
    */
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  /**
    * We also need something to run a parser
    */
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  /**
    * Need a way to parse strings
    *
    * should satisfy `run(string(s))(s) == Right(s)`
    */
  implicit def string(s: String): Parser[String]

  /**
    * Also want to be able to say `or`
    */
  def orString(s1: String, s2: String): Parser[String]

  /**
    * We can generalise this
    *
    * this should satisfy
    *
    * `run(or(string("abra"), string("cadabra")))("abra") == Right("abra")`
    * `run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")`
    */
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  /**
    * With these two (and the newly implicted string) Scala will automatically promote String to a Parser and we will
    * get infix operators
    *
    * so given `val P: Parsers`, we can `import P._` and create a parser like `"abra" | "cadabra"`, this will work for
    * all implementations of parsers
    */
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit  f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  /**
    * run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
    * run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
    * run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
    */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]


  /**
    * always succeeds with the value a
    */
  def succeed[A](a: A): Parser[A] = string("") map {_ => a}

  /**
    * should obey:
    * run(slice(('a'|'b').many))("aaba") results in Right("aaba")
    */
  def slice[A](p: Parser[A]): Parser[String]

  /**
    * so our char parser becomes (doesn't compile as yet)
    */
  //val numA: Parser[Int] = char('a').many.slice.map(_.size)

    /**
      * one or many, note this feels a lot like it should be p followed by many(p), so create product
      */
    def many1[A](p: Parser[A]): Parser[List[A]] = ???

    def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] = ???

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def many(p: Parser[A]): Parser[A] = ???
    def map[B](f: A => B): Parser[B] = ???
  }

  /**
    * Somewhere to keep laws about our functions
    *
    * note, the book uses stuff from chapter8 here, I'd like to use scalacheck
    */
  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)

    def succeedLaw[A](p: Parser[A], a: Parser[A])(in: Gen[String]): Prop = equal(succeed(p), a)(in)
  }
}