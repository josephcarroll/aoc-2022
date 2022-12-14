package joe.aoc

import scala.collection.mutable

object Day18 extends App {

  case class Voxel(x: Int, y: Int, z: Int)
  case class Node(location: Voxel, left: Option[Node], right: Option[Node])

  def createTree(voxels: Seq[Voxel], depth: Int): Option[Node] = {
    if (voxels.isEmpty) {
      None
    } else if (voxels.size == 1) {
      Some(Node(voxels.head, None, None))
    } else {
      def value(voxel: Voxel): Int = voxel.productElement(depth % 3).asInstanceOf[Int]
      val sorted = voxels.sortBy(value)
      val median = sorted.length / 2
      val (left, right) = sorted.splitAt(median)
      Some(Node(sorted(median), createTree(left, depth + 1), createTree(right.tail, depth + 1)))
    }
  }

  val input = Helpers.readInput(18)
  val voxels = input.map { line =>
    val Array(x, y, z) = line.split(",")
    Voxel(x.toInt, y.toInt, z.toInt)
  }.toSet

  def neighboursOf(voxel: Voxel): Seq[Voxel] = Seq(
    voxel.copy(x = voxel.x + 1, y = voxel.y, z = voxel.z),
    voxel.copy(x = voxel.x - 1, y = voxel.y, z = voxel.z),
    voxel.copy(x = voxel.x, y = voxel.y + 1, z = voxel.z),
    voxel.copy(x = voxel.x, y = voxel.y - 1, z = voxel.z),
    voxel.copy(x = voxel.x, y = voxel.y, z = voxel.z + 1),
    voxel.copy(x = voxel.x, y = voxel.y, z = voxel.z - 1)
  ).filter(v => v.x >= -1 && v.y >= -1 && v.z >= -1)
    .filter(v => v.x < 35 && v.y < 35 && v.z < 35)

  val neighbourCount = voxels.toSeq.map { voxel =>
    val possibleNeighbours = neighboursOf(voxel)
    6 - possibleNeighbours.count(voxels.contains)
  }
  println(neighbourCount.sum)

  val air = new mutable.HashSet[Voxel]()
  def dfs(location: Voxel): Unit = {
    if (!air.contains(location)) {
      air.add(location)
      val notLava = neighboursOf(location).filterNot(voxels.contains)
      notLava.foreach(dfs)
    }
  }

  dfs(Voxel(0, 0, 0))

  val touchingAir = voxels.toSeq.map { voxel =>
    val possibleNeighbours = neighboursOf(voxel)
    possibleNeighbours.count(air.contains)
  }
  println(touchingAir.sum)

}
