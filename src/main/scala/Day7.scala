package joe.aoc

import scala.util.parsing.combinator.RegexParsers

object Day7 extends App {

  trait Command
  case class CdCommand(path: String) extends Command
  case class LsCommand(responses: Seq[LsResponse]) extends Command
  case class LsResponse(size: Int, isDirectory: Boolean, name: String)

  object CommandParser extends RegexParsers {
    override def skipWhitespace: Boolean = true

    def cdCommand: Parser[CdCommand] = ("$" ~> "cd" ~> """[\w./]+""".r) ^^ CdCommand
    def lsCommand: Parser[LsCommand] = "$" ~> "ls" ~> rep1(dirResponse | fileResponse) ^^ LsCommand
    def dirResponse: Parser[LsResponse] = "dir" ~> "\\w+".r ^^ { name =>
      LsResponse(-1, isDirectory = true, name)
    }
    def fileResponse: Parser[LsResponse] = "\\d+".r ~ """[\w.]+""".r ^^ { case size ~ name =>
      LsResponse(size.toInt, isDirectory = false, name)
    }

    def command: Parser[Seq[Command]] = rep(cdCommand | lsCommand)

    def process(input: String): Seq[Command] = parse(command, input).get

  }

  val lines = Helpers.readInput(7)
  val parsed = CommandParser.process(lines.mkString("\n"))

  var currentParent = Seq[String]()
  val directorySizes = collection.mutable.HashMap[Seq[String], Int]()

  def populate(size: Int, directory: Seq[String]): Unit = {
    for (i <- 0 to directory.length) {
      val current = directory.take(i)
      if (directorySizes.contains(current)) {
        directorySizes.put(current, directorySizes(current) + size)
      } else {
        directorySizes.put(current, size)
      }
    }
  }

  parsed.foreach {
    case CdCommand("..") =>
      currentParent = currentParent.init
    case CdCommand(path) =>
      currentParent = currentParent :+ path
    case LsCommand(responses) =>
      responses.foreach {
        case LsResponse(size, isDirectory, _) =>
          if (!isDirectory) {
            populate(size, currentParent)
          }
      }
  }

  val matches = directorySizes.collect {
    case (_, size) if size <= 100000 => size
  }
  println(matches.sum)

  val used = directorySizes(Seq("/"))
  val total = 70000000
  val remaining = total - used
  val needed = 30000000 - remaining

  val candidates = directorySizes.collect {
    case (_, size) if size >= needed => size
  }
  println(candidates.min)
}
