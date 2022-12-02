package joe.aoc

object Day2 extends App {

  sealed abstract class Move(val worth: Int)
  case object Rock extends Move(1)
  case object Paper extends Move(2)
  case object Scissors extends Move(3)

  sealed trait State
  case object Lose extends State
  case object Draw extends State
  case object Win extends State

  case class Round(opponentMove: Move, response: Move, expected: State) {
    def score: Int = {
      val winScore = (opponentMove, response) match {
        case (x, y) if x == y => 3 // draw
        case (Rock, Scissors) => 0
        case (Paper, Rock) => 0
        case (Scissors, Paper) => 0
        case (Rock, Paper) => 6
        case (Paper, Scissors) => 6
        case (Scissors, Rock) => 6
      }
      response.worth + winScore
    }

    def newScore: Int = {
      (opponentMove, expected) match {
        case (Rock, Lose) => 0 + Scissors.worth
        case (Paper, Lose) => 0 + Rock.worth
        case (Scissors, Lose) => 0 + Paper.worth

        case (x, Draw) => 3 + x.worth

        case (Rock, Win) => 6 + Paper.worth
        case (Paper, Win) => 6 + Scissors.worth
        case (Scissors, Win) => 6 + Rock.worth
      }
    }
  }

  val lines = Helpers.readInput("day2-input.txt")
  val rounds = lines.map { line =>
    val Array(opponentMoveString, responseString) = line.split(" ")
    val opponentMove = opponentMoveString match {
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
    }
    val response = responseString match {
      case "X" => Rock
      case "Y" => Paper
      case "Z" => Scissors
    }
    val expectedState = responseString match {
      case "X" => Lose
      case "Y" => Draw
      case "Z" => Win
    }
    Round(opponentMove, response, expectedState)
  }

  println(rounds.map(_.score).sum)
  println(rounds.map(_.newScore).sum)

}
