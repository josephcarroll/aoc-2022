package joe.aoc

import scala.collection.mutable

object Day5 extends App {

  val items = Helpers.readInput(5)
  val (_, instructions) = items.span(_ != "")

  def run(reverse: Boolean): Unit = {
    val state = Seq(
      mutable.Stack("N", "H", "S", "J", "F", "W", "T", "D"),
      mutable.Stack("G", "B", "N", "T", "Q", "P", "R", "H"),
      mutable.Stack("V", "Q", "L"),
      mutable.Stack("Q", "R", "W", "S", "B", "N"),
      mutable.Stack("B", "M", "V", "T", "F", "D", "N"),
      mutable.Stack("R", "T", "H", "V", "B", "D", "M"),
      mutable.Stack("J", "Q", "B", "D"),
      mutable.Stack("Q", "H", "Z", "R", "V", "J", "N", "D"),
      mutable.Stack("S", "M", "H", "N", "B")
    )

    instructions.drop(1).foreach { instruction =>
      val pattern = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r
      val pattern(count, from, to) = instruction
      val taken = (1 to count.toInt).map(_ => state(from.toInt - 1).pop)
      val toReplace = if (reverse) taken.reverse else taken
      toReplace.foreach(state(to.toInt - 1).push)
    }

    state.foreach { stack =>
      print(stack.top)
    }
    println()
  }

  run(reverse = false)
  run(reverse = true)

}
