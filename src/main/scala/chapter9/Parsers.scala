package chapter9

/**
  * Let's create a Parser!
  *
  * note the weirdness going on with Parser[+_] - it means a type parameter for something that is itself a type
  * constructor
  */
trait Parsers[ParseError, Parser[+_]] {
  /**
    * Simpliest parser we can imagine - it reads a char and returns this Parser thing
    *
    * Parser returns a type on success and information about the failure if there is one
    *
    * should satisy `run(char(c))(c.toString) == Right(c)`
    */
  def char(c: Char): Parser[Char]

  /**
    * We also need something to run a parser
    */
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  /**
    * Need a way to parse strings
    *
    * should satisy `run(string(s))(s) == Right(s)`
    */
  def string(s: String): Parser[String]
}