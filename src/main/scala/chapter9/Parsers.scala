package chapter9

import chapter9.Parsers
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
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = ???

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
    * Exercise 9.3: many in terms of or, map2 and succeed
    * many(p) = tries running p, followed by many p.. until fails, then appends empty
    */
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) | succeed(List())
  /**
    * one or many, note this feels a lot like it should be p followed by many(p), so create product
    */
  def many1[A](p: Parser[A]): Parser[List[A]] = ???

  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] = ???

  /**
    * Exercise 9.1: using product, implement map2 and then use this to implement many1 in terms of many. Note, we could
    * also have chosen to make this primitive
    *
    * note: I had this worked out for ages, but couldn't get it to type check - turns out you need .tupled on f here,
    * didn't know that was a thing, but hey..
    */
  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    product(p, p2) map f.tupled

  def many1_map2[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def **[B >: A](p: Parser[A], p2: Parser[B]) = self.product(p,p2)

    def many = self.many(p)
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

    // Exercise 9.2: generate laws for product: actually don't think this is true for parsers, but would be for normal product
    def productLaw_commutes[A,B](p: Parser[A], p2: Parser[B])(in: Gen[String]): Prop = equal(product(p, p2), product(p2, p))(in)
  }
}