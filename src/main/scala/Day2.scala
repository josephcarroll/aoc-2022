package joe.aoc

object Day2 extends App {

  sealed abstract class Move(val worth: Int)
  case object Rock extends Move(1)
  case object Paper extends Move(2)
  case object Scissors extends Move(3)

  sealed abstract class State(val worth: Int)
  case object Lose extends State(0)
  case object Draw extends State(3)
  case object Win extends State(6)

  case class Round(opponentMove: Move, response: Move, expected: State) {
    def score: Int = {
      val winScore = (opponentMove, response) match {
        case (x, y) if x == y => Draw.worth
        case (Rock, Scissors) => Lose.worth
        case (Paper, Rock) => Lose.worth
        case (Scissors, Paper) => Lose.worth
        case (Rock, Paper) => Win.worth
        case (Paper, Scissors) => Win.worth
        case (Scissors, Rock) => Win.worth
      }
      response.worth + winScore
    }

    def newScore: Int = {
      (opponentMove, expected) match {
        case (x, Draw) => Draw.worth + x.worth
        case (Rock, Lose) => Lose.worth + Scissors.worth
        case (Paper, Lose) => Lose.worth + Rock.worth
        case (Scissors, Lose) => Lose.worth + Paper.worth
        case (Rock, Win) => Win.worth + Paper.worth
        case (Paper, Win) => Win.worth + Scissors.worth
        case (Scissors, Win) => Win.worth + Rock.worth
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
