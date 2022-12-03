package joe.aoc

object Day3 extends App {

  val items = Helpers.readInput("day3-input.txt")

  def toSet(input: String): Set[Char] = Set(input.toCharArray: _*)
  def priorityOf(input: Char): Int = {
    val asChar = input.asInstanceOf[Int] // ASCII
    if (input.isLower) asChar - 96 else asChar - 38
  }

  val priorities = items.map { rucksack =>
    val (left, right) = rucksack.splitAt(rucksack.length / 2)
    val overlap = toSet(left).intersect(toSet(right)).head
    priorityOf(overlap)
  }

  println(priorities.sum)

  val badges = items.grouped(3).map { case Seq(one, two, three) =>
    val overlap = toSet(one).intersect(toSet(two)).intersect(toSet(three)).head
    priorityOf(overlap)
  }

  println(badges.sum)

}
