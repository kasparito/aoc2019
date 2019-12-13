object Day5 extends Base(5) {

  def initialState: Array[Long] = inputLines.head.split(',').map(_.toLong)

  override def part1: Long = // 13087969
    IntcodeComputer.run(initialState, new SimpleHandler(Seq(1))).await.last

  override def part2: Long = // 14110739
    IntcodeComputer.run(initialState, new SimpleHandler(Seq(5))).await.head
}
