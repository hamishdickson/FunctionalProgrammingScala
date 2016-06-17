package chapter13

trait Files[A]
case class OpenRead(file: String) extends Files[HandleR]
case class OpenWrite(file: String) extends Files[HandleW]
case class ReadLines(file: String) extends Files[List[String]]
case class WriteLines(file: String, lines: List[String]) extends Files[Unit]

trait HandleR
trait HandleW

object Files {
}
