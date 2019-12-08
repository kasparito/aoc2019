object Day7 extends Base(7) {

  def initialState: Array[Int] = inputLines.head.split(',').map(_.toInt)

  trait State {
    def output: Seq[Int]
  }
  case class WaitingForInput(output: Seq[Int]) extends State
  case class Halted(output: Seq[Int]) extends State

  class IntcodeComputer(state: Array[Int]) {

    private var position = 0

    @scala.annotation.tailrec
    private def run(input: Seq[Int], output: Seq[Int]): State = {
      val instruction = state(position).toString
      val operation = instruction.takeRight(2).toInt
      val termTypes = instruction.dropRight(2).reverse.map(_.toString.toInt).padTo(2, 0)

      def term(i: Int): Int = if (termTypes(i - 1) == 1) state(position + i) else state(state(position + i))

      def set(i: Int, v: Int): Unit = state(state(position + i)) = v

      operation match {
        case 1 =>
          set(3, term(1) + term(2))
          position += 4
          run(input, output)
        case 2 =>
          set(3, term(1) * term(2))
          position += 4
          run(input, output)
        case 3 if input.isEmpty =>
          WaitingForInput(output)
        case 3 =>
          set(1, input.head)
          position += 2
          run(input.tail, output)
        case 4 =>
          val t = term(1)
          position += 2
          run(input, output :+ t)
        case 5 =>
          position = if (term(1) != 0) term(2) else position + 3
          run(input, output)
        case 6 =>
          position = if (term(1) == 0) term(2) else position + 3
          run(input, output)
        case 7 =>
          set(3, if (term(1) < term(2)) 1 else 0)
          position += 4
          run(input, output)
        case 8 =>
          set(3, if (term(1) == term(2)) 1 else 0)
          position += 4
          run(input, output)
        case 99 =>
          Halted(output)
      }
    }

    def run(input: Seq[Int]): State =
      run(input, Seq.empty)
  }

  override def part1: Int = // 880726
    (0 to 4).permutations.map(run).max

  override def part2: Int = // 4931744
    (5 to 9).permutations.map(run).max

  private def run(settings: Seq[Int]): Int = {
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
