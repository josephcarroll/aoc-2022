package joe.aoc

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph

object Day12 extends App {

  val lines = Helpers.readInput(12).map(_.toCharArray)

  case class Node(i: Int, j: Int, value: Char)

  object Node {
    def apply(i: Int, j: Int): Node = Node(i, j, lines(i)(j))
  }

  val graph = Graph[Node, DiEdge]()

  var start: Node = _
  var end: Node = _

  for (i <- lines.indices) {
    for (j <- lines.head.indices) {
      if (lines(i)(j) == 'S')
        start = Node(i, j)
      if (lines(i)(j) == 'E')
        end = Node(i, j)
      graph.add(Node(i, j))
    }
  }

  def weightOf(input: Node): Int = {
    if (input.value == 'S') {
      'a'.toInt
    } else if (input.value == 'E') {
      'z'.toInt
    } else {
      input.value.toInt
    }
  }

  for (i <- lines.indices) {
    for (j <- lines.head.indices) {
      val currentNode = Node(i, j)
      val up = if (j - 1 >= 0) Some(Node(i, j - 1)) else None
      val down = if (j + 1 < lines.head.length) Some(Node(i, j + 1)) else None
      val left = if (i - 1 >= 0) Some(Node(i - 1, j)) else None
      val right = if (i + 1 < lines.length) Some(Node(i + 1, j)) else None
      val reachable = Seq(up, down, left, right).flatten

      reachable.foreach { candidateNode =>
        val weight = weightOf(candidateNode) - weightOf(currentNode)
        if (weight == 1 || weight <= 0) {
          val edge = DiEdge(currentNode, candidateNode)
          graph.add(edge)
        }
      }
    }
  }

  val path = graph.get(start).shortestPathTo(graph.get(end))
  println(path.get.edges.size)

  val candidates = for {i <- lines.indices; j <- lines.head.indices} yield {
    val candidate = Node(i, j)
    if (Set('a', 'S').contains(candidate.value)) {
      val path = graph.get(candidate).shortestPathTo(graph.get(end))
      path.map(_.edges.size)
    } else {
      None
    }
  }

  println(candidates.flatten.min)

}
