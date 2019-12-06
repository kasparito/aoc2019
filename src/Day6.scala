object Day6 extends Base(6) {

  override def part1: Int = { // 295834
    val edges: Map[String, Iterable[String]] = inputLines
      .map(_.split(')') match { case Array(a, b) => a -> b })
      .groupKeyValue

    val root = (edges.keySet -- edges.values.flatten).head

    def count(depth: Int, node: String): Int =
      depth + edges
        .get(node)
        .map(_.map(count(depth + 1, _)).sum)
        .getOrElse(0)

    count(0, root)
  }

  override def part2: Int = { // 361
    val parents: Map[String, String] = inputLines
      .map(_.split(')') match { case Array(a, b) => b -> a })
      .toMap

    def path(node: String): List[String] =
      parents.get(node).map(parent => node :: path(parent)).getOrElse(Nil)

    val you = path("YOU").reverse
    val santa = path("SAN").reverse

    @scala.annotation.tailrec
    def distance(path1: List[String], path2: List[String]): Int =
      (path1, path2) match {
        case (head1 :: tail1, head2 :: tail2) if head1 != head2 =>
          tail1.size + tail2.size
        case (_ :: tail1, _ :: tail2) =>
          distance(tail1, tail2)
      }

    distance(you, santa)
  }
}
