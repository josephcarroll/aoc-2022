package joe.aoc

import scala.collection.mutable

object Day6 extends App {

  val line = Helpers.readInput(6).head

  def find(markerLength: Int): Int = {
    val matched = line.sliding(markerLength).zipWithIndex.collectFirst {
      case (message, index) if Set.from(message).size == markerLength =>
        index
    }
    matched.get + markerLength
  }

  println(find(4))
  println(find(14))

}
