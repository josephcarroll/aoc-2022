package joe.aoc

object Day10 extends App {

  val lines = Helpers.readInput(10)

  sealed trait Op

  case object Noop extends Op

  case class AddX(value: Int) extends Op


  def readOp(input: String): Op = input.split(" ") match {
    case Array("noop") => Noop
    case Array("addx", value) => AddX(value.toInt)
  }

  var x = 1
  var cycle = 1

  val cycleChecks = Set(20, 60, 100, 140, 180, 220)
  var signalStrength = 0

  def checkState(): Unit = {
    if (cycleChecks.contains(cycle)) {
      signalStrength += cycle * x
    }
  }

  lines.map(readOp).foreach {
    case Noop =>
      cycle += 1
      checkState()
    case AddX(value) =>
      cycle += 1
      checkState()
      cycle += 1
      x += value
      checkState()
  }

  println(signalStrength)

  cycle = 0
  x = 1

  def render(): Unit = {
    if (cycle % 40 == 0 && cycle != 0)
      println()

    val sprite = Set(x - 1, x, x + 1)
    if (sprite.contains(cycle % 40)) {
      print("#")
    } else {
      print(".")
    }
  }

  lines.map(readOp).foreach {
    case Noop =>
      render()
      cycle += 1
    case AddX(value) =>
      render()
      cycle += 1
      render()
      x += value
      cycle += 1
  }
  println()

}
