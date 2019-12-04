object Day3 extends Base(3) {

  type Position = (Int, Int)

  object Move {
    def apply(c: Char): Move = c match {
      case 'R' => Right
      case 'L' => Left
      case 'U' => Up
      case 'D' => Down
    }
  }
  sealed trait Move {
    val dx: Int
    val dy: Int
    def move(p: Position): Position = (p._1 + dx, p._2 + dy)
  }
  object Right extends Move { val dx = 1; val dy = 0 }
  object Left extends Move { val dx = -1; val dy = 0 }
  object Up extends Move { val dx = 0; val dy = 1 }
  object Down extends Move { val dx = 0; val dy = -1 }

  def toMoves(s: String): Seq[Move] =
    if (s.isEmpty) Seq.empty else {
      val move = Move(s.head)
      val n = s.tail.takeWhile(_.isDigit).toInt
      Seq.fill(n)(move) ++: toMoves(s.dropWhile(_ != ',').tail)
    }

  def manhattanDistance(position: Position): Int =
    math.abs(position._1) + math.abs(position._2)

  override def part1: Int = { // 709
    val List(wire1, wire2) = inputLines.map {
      line =>
        toMoves(line)
          .scanLeft((0, 0))((position, move) => move.move(position))
          .toSet
    }
    wire1
      .intersect(wire2)
      .map(manhattanDistance)
      .filterNot(_ == 0)
      .min
  }

  override def part2: Int = {
    val List(wire1, wire2) = inputLines.map {
      line =>
        toMoves(line)
          .scanLeft( (0, 0) -> 0 ) { case ((position, steps), move) => move.move(position) -> (steps + 1) }
          .toMap
    }
    wire1.keySet
      .intersect(wire2.keySet)
      .map(p => wire1(p) + wire2(p))
      .filterNot(_ == 0)
      .min
  }
}
