package joe.aoc

import scala.collection.mutable.ArrayBuffer

object Day1 extends App {

  val input = io.Source.fromResource("day1-input.txt").getLines()
  val elves = new ArrayBuffer[ArrayBuffer[Int]]()
  var current = new ArrayBuffer[Int]()
  input.foreach { line =>
    if (line == "") {
      current = new ArrayBuffer[Int]()
      elves.append(current)
    } else {
      current.append(line.toInt)
    }
  }

  println(elves.map(_.sum).max)
  println(elves.map(_.sum).sorted.reverse.take(3).sum)

}