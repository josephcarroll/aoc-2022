package joe.aoc

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

object Day11 extends App {

  val input = Helpers.readEntireInput(11)

  sealed trait Op

  case class Square() extends Op

  case class Add(value: Long) extends Op

  case class Multiply(value: Long) extends Op

  case class Monkey(index: Int,
                    items: ArrayBuffer[Long],
                    operation: Op,
                    testDivider: Int,
                    trueThrow: Int,
                    falseThrow: Int)

  object MonkeyParser extends RegexParsers with JavaTokenParsers {

    def square: Parser[Square] = "*" ~> "old" ^^ {
      _ => Square()
    }

    def add: Parser[Add] = "+" ~> wholeNumber ^^ {
      num => Add(num.toLong)
    }

    def multiply: Parser[Multiply] = "*" ~> wholeNumber ^^ {
      num => Multiply(num.toLong)
    }

    def operator: Parser[Op] = square | add | multiply

    def monkey: Parser[Monkey] = "Monkey" ~
      wholeNumber ~ ":" ~ "Starting items:" ~
      rep1sep(wholeNumber, ",") ~ "Operation: new = old" ~ operator ~
      "Test: divisible by" ~ wholeNumber ~
      "If true: throw to monkey" ~ wholeNumber ~
      "If false: throw to monkey" ~ wholeNumber ^^ {
      case _ ~ monkeyNum ~ _ ~ _ ~ startingNums ~ _ ~ operator ~ _ ~ testNum ~ _ ~ trueNum ~ _ ~ falseNum =>
        Monkey(
          monkeyNum.toInt,
          ArrayBuffer.from(startingNums.map(_.toInt)),
          operator,
          testNum.toInt,
          trueNum.toInt,
          falseNum.toInt)
    }

    def monkeys: Parser[Seq[Monkey]] = rep(monkey)

    def process(input: String): Seq[Monkey] = parse(monkeys, input).get
  }

  val monkeys = MonkeyParser.process(input).toArray
  val monkeyCounts = mutable.HashMap.from(monkeys.map(m => m.index -> 0L))

  val mod = monkeys.map(_.testDivider).product

  for (i <- 1 to 10000) {
    for (j <- monkeys.indices) {
      val currentMonkey = monkeys(j)
      for (x <- currentMonkey.items) {
        monkeyCounts(currentMonkey.index) += 1
        val worryLevel = currentMonkey.operation match {
          case Square() => x * x
          case Add(value) => x + value
          case Multiply(value) => x * value
        }
        val better = worryLevel % mod // Math.floor(worryLevel.doubleValue / 3.0).toInt
        val target = if (better % currentMonkey.testDivider == 0) currentMonkey.trueThrow else currentMonkey.falseThrow
        monkeys(target).items.append(better)
      }
      currentMonkey.items.dropInPlace(currentMonkey.items.length)
    }
  }

  val topTwo = monkeyCounts.values.toSeq.sorted.reverse.take(2)
  println(topTwo.product)

}
