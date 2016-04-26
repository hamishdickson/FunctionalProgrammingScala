package chapter13

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
}
