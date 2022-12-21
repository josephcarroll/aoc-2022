package joe.aoc

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
  val inputVoxels = input.map { line =>
    val Array(x, y, z) = line.split(",")
    Voxel(x.toInt, y.toInt, z.toInt)
  }
  val tree = createTree(inputVoxels, depth = 0)

  println(inputVoxels.minBy(_.x))
  println(inputVoxels.minBy(_.y))
  println(inputVoxels.minBy(_.z))
  println(inputVoxels.maxBy(_.x))
  println(inputVoxels.maxBy(_.y))
  println(inputVoxels.maxBy(_.z))

  def findInGraph(tree: Option[Node])(voxel: Voxel): Boolean = tree match {
    case None => false
    case Some(node) =>
      if (node.location == voxel) {
        true
      } else {
        findInGraph(node.left)(voxel) || findInGraph(node.right)(voxel)
      }
  }

  def alongAxis(voxel: Voxel, tree: Option[Node], check: (Voxel, Voxel) => Boolean): Seq[Voxel] = tree match {
    case None => Seq()
    case Some(node) =>
      val matched = if (check(node.location, voxel) && node.location != voxel) {
        Seq(node.location)
      } else {
        Seq()
      }
      matched ++ alongAxis(voxel, node.left, check) ++ alongAxis(voxel, node.right, check)
  }

  def neighboursOf(voxel: Voxel): Seq[Voxel] = Seq(
    voxel.copy(x = voxel.x + 1, y = voxel.y, z = voxel.z),
    voxel.copy(x = voxel.x - 1, y = voxel.y, z = voxel.z),
    voxel.copy(x = voxel.x, y = voxel.y + 1, z = voxel.z),
    voxel.copy(x = voxel.x, y = voxel.y - 1, z = voxel.z),
    voxel.copy(x = voxel.x, y = voxel.y, z = voxel.z + 1),
    voxel.copy(x = voxel.x, y = voxel.y, z = voxel.z - 1)
  )

  val neighbourCount = inputVoxels.map { voxel =>
    val possibleNeighbours = neighboursOf(voxel)
    6 - possibleNeighbours.count(findInGraph(tree))
  }
  println(neighbourCount.sum)

  // internal + borders actual voxel
  val internalAirCandidates = inputVoxels.flatMap(neighboursOf).toSet -- inputVoxels.toSet
  val internalAirPockets = internalAirCandidates.filter { candidate =>
    val xAligned = alongAxis(candidate, tree, (l, r) => l.y == r.y && l.z == r.z)
    val yAligned = alongAxis(candidate, tree, (l, r) => l.x == r.x && l.z == r.z)
    val zAligned = alongAxis(candidate, tree, (l, r) => l.x == r.x && l.y == r.y)
    xAligned.exists(_.x < candidate.x) && xAligned.exists(_.x > candidate.x) && yAligned.exists(_.y < candidate.y) && yAligned.exists(_.y > candidate.y) && zAligned.exists(_.z < candidate.z) && zAligned.exists(_.z > candidate.z)
  }

  val externalSurfaceArea = inputVoxels.map { voxel =>
    val possibleNeighbours = neighboursOf(voxel)
    val neighboursSolidOrPocket = possibleNeighbours.count { pn =>
      findInGraph(tree)(pn) || internalAirPockets.contains(pn)
    }
    6 - neighboursSolidOrPocket
  }
  // assumption about looking in all directions is wrong! Think about a pipe/bend
  println(externalSurfaceArea.sum) // not 240

}
