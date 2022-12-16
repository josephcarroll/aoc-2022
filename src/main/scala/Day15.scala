package joe.aoc

import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters.MapHasAsScala

object Day15 extends App {

  val lines = Helpers.readInput(15)
  case class Point(x: Int, y: Int)
  case class Combo(sensor: Point, beacon: Point)

  val combos = lines.map { line =>
    val pattern = """Sensor at x=([-\d]+), y=([-\d]+): closest beacon is at x=([-\d]+), y=([-\d]+)""".r
    val pattern(sx, sy, bx, by) = line
    Combo(Point(sx.toInt, sy.toInt), Point(bx.toInt, by.toInt))
  }

  def distanceBetween(a: Point, b: Point) = Math.abs(a.x - b.x) + Math.abs(a.y - b.y)

  def checkYLine(yCheck: Int): Set[Int] = {
    val notXs = mutable.HashSet[Point]()
    combos.foreach { case Combo(sensor, beacon) =>
      val distanceToCheck = distanceBetween(sensor, beacon)
      val xRemaining = distanceToCheck - Math.abs(sensor.y - yCheck)
      for (x <- 0 to xRemaining) {
        val possiblePoints = Seq(
          Point(sensor.x + x, yCheck),
          Point(sensor.x - x, yCheck)
        )
        possiblePoints.foreach(notXs.add)
      }
    }
    combos.collect { case c if c.beacon.y == yCheck => c.beacon }.foreach(notXs.remove)
    notXs.map(_.x).toSet
  }

//  println(checkYLine(2000000).size)

  val bobMax = 4000000

  for (y <- 0 to bobMax) {

    val intervals = combos.flatMap { case Combo(sensor, beacon) =>
      val totalDistance = distanceBetween(sensor, beacon)
      val yDistance = Math.abs(sensor.y - y)
      val remainingDistance = totalDistance - yDistance
      val start = sensor.x - remainingDistance
      val end = sensor.x + remainingDistance
      if (start < end) {
        Some((Math.max(0, start), Math.min(end, bobMax)))
      } else {
        None
      }
    }

    val sorted = intervals.sorted
    var maxSeen = sorted.head._1
    sorted.foreach { case (start, end) =>
      if (start > maxSeen) {
        println(maxSeen)
        println(y)
        println((maxSeen.longValue * 4000000) + y)
      }
      maxSeen = Math.max(maxSeen, end)
    }
  }

}
