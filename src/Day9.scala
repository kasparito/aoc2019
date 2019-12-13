object Day9 extends Base(9) {

  def initialState: Array[Long] = inputLines.head.split(',').map(_.toLong)

  override def part1: Long = //
    IntcodeComputer.run(initialState, new SimpleHandler(Seq(1))).await.head
}
