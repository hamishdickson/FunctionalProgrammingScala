package chapter9

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
  def char(c: Char): Parser[Char]

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

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
  }
}