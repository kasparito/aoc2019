object Day9 extends Base(9) {

  def initialState: Array[Long] = inputLines.head.split(',').map(_.toLong)

  override def part1: Long = // 3454977209
    IntcodeComputer.run(initialState, new SimpleHandler(Seq(1))).await.head

  override def part2: Long = // 50120
    IntcodeComputer.run(initialState, new SimpleHandler(Seq(2))).await.head
}
