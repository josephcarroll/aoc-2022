package joe.aoc

import scala.annotation.tailrec

object Helpers {

  def readInput(name: String): Seq[String] = {
    io.Source.fromResource(name).getLines().toSeq
  }

  def splitCollection[T](input: Seq[T], sentinelValue: T): Seq[Seq[T]] = {
    @tailrec def split(result: Seq[Seq[T]], remainder: Seq[T]): Seq[Seq[T]] = {
      if (remainder.isEmpty) {
        result
      } else {
        val (l, r) = remainder.span(_ != sentinelValue)
        split(result :+ l, r.drop(1))
      }
    }
    split(Seq(), input)
  }

}
