package chapter9

import org.scalacheck._

import scala.util.matching.Regex

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
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

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
    *
    * Exercise 9.4: Implement listOfN using map2 and succeed
    */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN (n - 1, p) ) (_ :: _)

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
    *
    * There is a problem with this implementation - it never terminates! It's recursive in many - the way to fix this
    * is to make map2 and product non-strict in p2
    *
    * If you think about it, this makes a lot of sense for product anyway - if parser 1 fails, you don't want to
    * evaluate parser 2
    */
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) | succeed(List())

  /**
    * one or many, note this feels a lot like it should be p followed by many(p), so create product
    */
  def many1[A](p: Parser[A]): Parser[List[A]] = ???

  /**
    * Exercise 9.7: Implement product and map2 in terms of flatMap
    */
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => p2 map(b => (a,b)))

  def map2UsingFlatMap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    flatMap(p)(a => p2 map(b => f(a,b)))

  /**
    * Exercise 9.1: using product, implement map2 and then use this to implement many1 in terms of many. Note, we could
    * also have chosen to make this primitive
    *
    * note: I had this worked out for ages, but couldn't get it to type check - turns out you need .tupled on f here,
    * didn't know that was a thing, but hey..
    */
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    product(p, p2) map f.tupled

  def many1_map2[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  /**
    * Exercise 9.6: Implement case sensitive parser
    */
  implicit def regex(r: Regex): Parser[String]

  /**
    * Exercise 9.8: express map in terms of flatmap
    */
  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(f andThen succeed)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def **[B >: A](p: Parser[A], p2: Parser[B]) = self.product(p,p2)

    def many = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*[B](p2: => Parser[B]) = self.skipR(p, p2)
    def token = self.token(p)
    def sep(separator: Parser[Any]) = self.sep(p, separator)
    def sep1(separator: Parser[Any]) = self.sep1(p, separator)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
    def opL(op: Parser[(A,A) => A]): Parser[A] = self.opL(p)(op)
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


  /**
    * Exercise 9.9 - crazy hard - write a parser for json
    */
  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  /** Sequences two parsers, ignoring the result of the first.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_,b) => b)


  /** Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a,b) => a)

  def attempt[A](p: Parser[A]): Parser[A]

  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = "\\s*".r

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p,p2) or succeed(List())

  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  /** Parses a sequence of left-associative binary operators with the same precedence. */
  def opL[A](p: Parser[A])(op: Parser[(A,A) => A]): Parser[A] =
    map2(p, many(op ** p))((h,t) => t.foldLeft(h)((a,b) => b._1(a,b._2)))
  
}