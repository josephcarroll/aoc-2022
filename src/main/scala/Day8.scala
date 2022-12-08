package joe.aoc

import scala.util.control.Breaks.{break, breakable}

object Day8 extends App {

  val lines = Helpers.readInput(8)

  val grid = lines.map(_.toCharArray.toSeq.map(_.toString.toInt))

  var visible = 0
  for (i <- 1 until grid.length - 1) {
    for (j <- 1 until grid.length - 1) {
      val candidateHeight = grid(i)(j)
      val a = (0 until i).map(grid(_)(j)).forall(_ < candidateHeight)
      val b = (i + 1 until grid.length).map(grid(_)(j)).forall(_ < candidateHeight)
      val c = (0 until j).map(grid(i)(_)).forall(_ < candidateHeight)
      val d = (j + 1 until grid.length).map(grid(i)(_)).forall(_ < candidateHeight)
      visible += (if (a || b || c || d) 1 else 0)
    }
  }
  visible += (grid.length * 2) + ((grid.length - 2) * 2)
  println(visible)

  var scenicScores = Seq[Int]()
  for (i <- grid.indices) {
    for (j <- grid.indices) {
      val candidateHeight = grid(i)(j)
      var lScore = 0
      var rScore = 0
      var uScore = 0
      var dScore = 0

      breakable {
        for (x <- Range.inclusive(i - 1, 0, -1)) {
          lScore += 1
          if (grid(x)(j) >= candidateHeight) {
            break
          }
        }
      }
      breakable {
        for (x <- Range.inclusive(i + 1, grid.length - 1)) {
          rScore += 1
          if (grid(x)(j) >= candidateHeight) {
            break
          }
        }
      }
      breakable {
        for (y <- Range.inclusive(j - 1, 0, -1)) {
          uScore += 1
          if (grid(i)(y) >= candidateHeight) {
            break
          }
        }
      }
      breakable {
        for (y <- Range.inclusive(j + 1, grid.length - 1)) {
          dScore += 1
          if (grid(i)(y) >= candidateHeight) {
            break
          }
        }
      }

      scenicScores = scenicScores :+ lScore * rScore * uScore * dScore
    }
  }
  println(scenicScores.max)

}
