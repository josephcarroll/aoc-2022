package joe.aoc

object Day6 extends App {

  val line = Helpers.readEntireInput(6)

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
