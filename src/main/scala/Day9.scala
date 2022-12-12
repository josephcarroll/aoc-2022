package joe.aoc

import scala.collection.mutable

object Day9 extends App {

  val lines = Helpers.readInput(9)

  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  val steps = lines.flatMap { line =>
    val Array(dir, count) = line.split(" ")
    val fill = Array.fill[Direction](count.toInt)(_)
    dir match {
      case "U" => fill(Up)
      case "D" => fill(Down)
      case "L" => fill(Left)
      case "R" => fill(Right)
    }
  }

  case class Point(x: Int, y: Int) {
    def -(that: Point): Point = {
      Point(this.x - that.x, this.y - that.y)
    }
  }

  def takeStep(step: Direction): Point = step match {
    case Up =>
      head.copy(y = head.y + 1)
    case Down =>
      head.copy(y = head.y - 1)
    case Left =>
      head.copy(x = head.x - 1)
    case Right =>
      head.copy(x = head.x + 1)
  }

  var head = Point(0, 0)
  var tail = Point(0, 0)
  var seen = new mutable.HashSet[Point]()
  seen.add(tail)

  steps.foreach { step =>
    head = takeStep(step)
    val adjacency = head - tail
    val adjacent = adjacency.x.abs < 2 && adjacency.y.abs < 2

    if(!adjacent) {
      val dragDelta = Point(adjacency.x.sign, adjacency.y.sign)
      tail = Point(tail.x + dragDelta.x, tail.y + dragDelta.y)
    }

    seen.add(tail)
  }

  println(seen.size)

  seen = new mutable.HashSet[Point]()
  var rope = Array.fill(9)(Point(0, 0))
  seen.add(Point(0, 0))
  head = Point(0, 0)

  steps.foreach { step =>
    head = takeStep(step)

    for(i <- rope.indices) {
      val currentTail = rope(i)
      val currentHead = if (i == 0) head else rope(i - 1)
      val adjacency = currentHead - currentTail
      val adjacent = adjacency.x.abs < 2 && adjacency.y.abs < 2

      if (!adjacent) {
        val dragDelta = Point(adjacency.x.sign, adjacency.y.sign)
        rope(i) = Point(currentTail.x + dragDelta.x, currentTail.y + dragDelta.y)
      }

      if (i == rope.length - 1) seen.add(currentTail)
    }
  }

  println(seen.size)

}
