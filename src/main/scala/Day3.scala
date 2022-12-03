package joe.aoc

object Day3 extends App {

  println("day 3!")
  val items = Helpers.readInput("day3-input.txt")

  val priorities = items.map { rucksack =>
    val (left, right) = rucksack.splitAt(rucksack.length / 2)
    val overlap = Set(left.toCharArray: _*).intersect(Set(right.toCharArray: _*)).head
    if (overlap.isLower) overlap.asInstanceOf[Int] - 96 else overlap.asInstanceOf[Int] - 38
  }

  println(priorities.sum)

  val badges = items.grouped(3).map { case Seq(one, two, three) =>
    val overlap = Set(one.toCharArray: _*).intersect(Set(two.toCharArray: _*)).intersect(Set(three.toCharArray: _*)).head
    if (overlap.isLower) overlap.asInstanceOf[Int] - 96 else overlap.asInstanceOf[Int] - 38
  }

  println(badges.sum)

}
