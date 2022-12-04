package joe.aoc

object Day4 extends App {

  val items = Helpers.readInput(4)

  val parsed = items.map { item =>
    val Array(left, right) = item.split(",")
    val Array(ll, lr) = left.split("-")
    val Array(rl, rr) = right.split("-")
    (ll.toInt, lr.toInt, rl.toInt, rr.toInt)
  }

  val contained = parsed.count { case (a, b, c, d) =>
    val leftContainsRight = a <= c && b >= d
    val rightContainsLeft = c <= a && d >= b
    leftContainsRight || rightContainsLeft
  }
  println(contained)

  val overlapped = parsed.count { case (a, b, c, d) =>
    val leftOutsideRight = a > d || b < c
    val rightOutsideLeft = c > b || d < a
    !(leftOutsideRight && rightOutsideLeft)
  }
  println(overlapped)

}
