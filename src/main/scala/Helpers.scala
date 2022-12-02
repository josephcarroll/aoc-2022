package joe.aoc

import scala.collection.mutable.ArrayBuffer

object Helpers {

  def readInput(name: String): Iterator[String] = {
    io.Source.fromResource(name).getLines()
  }

  def splitCollection[T](input: Iterator[T], sentinelValue: T): Seq[Seq[T]] = {
    val results = new ArrayBuffer[ArrayBuffer[T]]()
    var current = new ArrayBuffer[T]()
    input.foreach { line =>
      if (line == sentinelValue) {
        current = new ArrayBuffer[T]()
        results.append(current)
      } else {
        current.append(line)
      }
    }
    results.map(_.toSeq).toSeq
  }

}
