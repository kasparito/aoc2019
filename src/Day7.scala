object Day7 extends Base(7) {

  def initialState: Array[Long] = inputLines.head.split(',').map(_.toLong)

  override def part1: Long = // 880726
    (0 to 4).permutations.map(run).max

  override def part2: Long = // 4931744
    (5 to 9).permutations.map(run).max

  private def run(settings: Seq[Int]): Long = {
    val computers = IndexedSeq.fill(settings.size)(new IntcodeComputer(initialState))
    computers.zip(settings).foreach {
      case (computer, phaseSetting) =>
        computer.run(Seq(phaseSetting))
    }
    var state: State = WaitingForInput(Seq(0))
    while (state.isInstanceOf[WaitingForInput]) {
      state = computers.foldLeft(state) {
        case (inputState, computer) =>
          computer.run(inputState.output)
      }
    }
    state.output.head
  }
}
