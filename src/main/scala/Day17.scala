package joe.aoc

import scala.collection.mutable

object Day17 extends App {

  case class Point(x: Int, y: Int) {
    def +(that: Point): Point = Point(x + that.x, y + that.y)
  }

  case class Shape(points: Seq[Point]) {

    def translate(by: Point): Shape = {
      Shape(points.map(_ + by))
    }

    def overlaps(grid: mutable.HashSet[Point]): Boolean = {
      points.exists(grid.contains)
    }

    def outOfBounds: Boolean = {
      points.exists(p => p.x < 0 || p.x >= 7)
    }

    def bottomPoint: Int = points.maxBy(_.y).y

  }

  // ####
  val one = Shape(Seq(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)))
  // .#.
  // ###
  // .#.
  val two = Shape(Seq(Point(1, 0), Point(0, 1), Point(1, 1), Point(2, 1), Point(1, 2)))
  // ..#
  // ..#
  // ###
  val three = Shape(Seq(Point(2, 0), Point(2, 1), Point(0, 2), Point(1, 2), Point(2, 2)))
  // #
  // #
  // #
  // #
  val four = Shape(Seq(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3)))
  // ##
  // ##
  val five = Shape(Seq(Point(0, 0), Point(0, 1), Point(1, 0), Point(1, 1)))

  val floor = (0 until 7).map(Point(_, 4))

  val rockTypes = Seq(one, two, three, four, five)

  val input = Helpers.readEntireInput(17)
  val state = new mutable.HashSet[Point]()
  floor.foreach(state.add)

  def lowestPoint: Int = state.maxBy(_.y).y
  def highestPoint: Int = state.minBy(_.y).y

  sealed trait Jet
  case object Left extends Jet
  case object Right extends Jet
  def jetAt(i: Int): Jet = input(i % input.length) match {
    case '<' => Left
    case '>' => Right
  }

  var i = 0
  (0 until 2022).map(i => rockTypes(i % rockTypes.length)).foreach { rock =>
    var currentRock = rock.translate(Point(2, highestPoint - 4 - rock.bottomPoint))
    var resting = false
    do {
      // push
      val pushed = jetAt(i) match {
        case Left  => currentRock.translate(Point(-1, 0))
        case Right => currentRock.translate(Point(1, 0))
      }
      if (!pushed.outOfBounds && !pushed.overlaps(state)) {
        currentRock = pushed
      }
      // drop
      val dropped = currentRock.translate(Point(0, 1))
      if (dropped.overlaps(state)) {
        resting = true
        currentRock.points.foreach(state.add)
      } else {
        currentRock = dropped
      }
      i += 1
    } while(!resting)
  }

  println(lowestPoint - highestPoint)

}
