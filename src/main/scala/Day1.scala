package joe.aoc

object Day1 extends App {

  val input = Helpers.readInput(1)
  val elves = Helpers.splitBySentinel(input, sentinelValue = "").map(_.map(_.toInt))

  println(elves.map(_.sum).max)
  println(elves.map(_.sum).sorted.reverse.take(3).sum)

}