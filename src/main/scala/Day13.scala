package joe.aoc

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

object Day13 extends App {

  val lines = Helpers.readInput(13)

  val pairs = Helpers.splitBySentinel(lines, "")

  sealed trait PacketPiece
  case class PacketValue(value: Int) extends PacketPiece {
    override def toString: String = value.toString
  }
  case class PacketList(values: Seq[PacketPiece]) extends PacketPiece {
    override def toString: String = s"[${values.mkString(", ")}]"
  }

  object PacketParser extends RegexParsers with JavaTokenParsers {

    def value: Parser[PacketValue] = wholeNumber ^^ {
      v => PacketValue(v.toInt)
    }

    def list: Parser[PacketList] = "[" ~> repsep(list | value, ",") <~ "]" ^^ {
      v => PacketList(v)
    }

    def process(input: String): PacketList = parse(list, input).get
  }

  def compare(left: PacketPiece, right: PacketPiece): Option[Boolean] = (left, right) match {
    case (PacketValue(l), PacketValue(r)) =>
      if (l < r) {
        Some(true)
      } else if (l > r) {
        Some(false)
      } else {
        None
      }
    case (PacketList(l), PacketList(r)) =>
      val checked = l.zip(r).map { case (x, y) => compare(x, y) }
      val matched = checked.collectFirst {
        case Some(x) => x
      }
      matched.orElse {
        if (r.length < l.length) {
          Some(false)
        } else if (r.length > l.length) {
          Some(true)
        } else {
          None
        }
      }
    case (l: PacketList, r: PacketValue) =>
      compare(l, PacketList(Seq(r)))
    case (l: PacketValue, r: PacketList) =>
      compare(PacketList(Seq(l)), r)
  }

  var counter = 1
  var total = 0
  pairs.foreach { case Seq(a, b) =>
    val left = PacketParser.process(a)
    val right = PacketParser.process(b)
    val result = compare(left, right)
    if (result.get) {
      total += counter
    }
    counter += 1
  }

  println(total)

  val sorted = lines.filterNot(_ == "").map(PacketParser.process).sortWith { case (x, y) =>
    compare(x, y).get
  }

  val a = sorted.indexOf(PacketList(Seq(PacketList(Seq(PacketValue(2)))))) + 1
  val b = sorted.indexOf(PacketList(Seq(PacketList(Seq(PacketValue(6)))))) + 1
  println(a * b)

}
