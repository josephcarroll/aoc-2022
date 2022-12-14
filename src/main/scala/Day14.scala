package joe.aoc

import scala.collection.mutable

object Day14 extends App {

  val input = Helpers.readInput(14)

  case class Point(x: Int, y: Int) {
    def +(that: Point): Point = {
      Point(this.x + that.x, this.y + that.y)
    }
  }

  case class Line(start: Point, end: Point)

  val lines = input.flatMap { line =>
    line.split(" -> ").sliding(2).map { case Array(a, b) =>
      val Array(ax, ay) = a.split(",")
      val Array(bx, by) = b.split(",")
      Line(Point(ax.toInt, ay.toInt), Point(bx.toInt, by.toInt))
    }
  }

  val points = lines.flatMap { line =>
    if (line.start.x == line.end.x) {
      // horizontal
      val range = Range.inclusive(line.start.y, line.end.y, if (line.start.y < line.end.y) 1 else -1)
      range.map(y => Point(line.start.x, y))
    } else {
      // vertical
      val range = Range.inclusive(line.start.x, line.end.x, if (line.start.x < line.end.x) 1 else -1)
      range.map(x => Point(x, line.start.y))
    }
  }
  val lastY = points.maxBy(_.y).y

  val floor = (-10000 to 10000).map(x => Point(x, lastY + 2))
  val stage = mutable.HashSet.from(points ++ floor)

  var i = 0
  var answer = -1
  while (answer == -1) {
    var sand = Point(500, 0)
    var resting = false
    var blockedOnStart = true
    while (!resting) {
      val down = sand + Point(0, 1)
      val left = sand + Point(-1, 1)
      val right = sand + Point(1, 1)
      val newLocation = Seq(down, left, right).find(p => !stage.contains(p))
      newLocation match {
        case None =>
          stage.add(sand)
          resting = true
          i += 1
          if (blockedOnStart) {
            answer = 1
          }
        case Some(location) =>
          blockedOnStart = false
          sand = location
      }
    }
  }

  println(i)

}
