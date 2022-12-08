package joe.aoc

object AllDays extends App {

  val days: Seq[App] = Seq(Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8)
  days.zipWithIndex.foreach { case (app, i) =>
    println(s"Day ${i + 1}")
    app.main(Array())
  }

}
