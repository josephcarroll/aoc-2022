package joe.aoc

object AllDays extends App {

  val days: Seq[App] = Seq(Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10, Day11, Day12, Day13, Day14)
  days.zipWithIndex.foreach { case (app, i) =>
    println()
    val header = s"Day ${i + 1}"
    println("=" * header.length)
    println(header)
    println("=" * header.length)
    println()
    app.main(Array())
  }

}
