object Day5 extends Base(5) {

  def initialState: Array[Int] = inputLines.head.split(',').map(_.toInt)

  class IntcodeComputer(state: Array[Int]) {

    @scala.annotation.tailrec
    private def run(position: Int, input: Int): Int = {
      val instruction = state(position).toString
      val operation = instruction.takeRight(2).toInt
      val termTypes = instruction.dropRight(2).reverse.map(_.toString.toInt).padTo(2, 0)
      def term(i: Int): Int = if (termTypes(i - 1) == 1) state(position + i) else state(state(position + i))
      def set(i: Int, v: Int): Unit = state(state(position + i)) = v

      operation match {
        case 1 =>
          set(3, term(1) + term(2))
          run(position + 4, input)
        case 2 =>
          set(3, term(1) * term(2))
          run(position + 4, input)
        case 3 =>
          set(1, input)
          run(position + 2, input)
        case 4 =>
          run(position + 2, term(1))
        case 5 =>
          run(if (term(1) != 0) term(2) else position + 3, input)
        case 6 =>
          run(if (term(1) == 0) term(2) else position + 3, input)
        case 7 =>
          set(3, if (term(1) < term(2)) 1 else 0)
          run(position + 4, input)
        case 8 =>
          set(3, if (term(1) == term(2)) 1 else 0)
          run(position + 4, input)
        case 99 =>
          input
      }
    }

    def run(input: Int): Int =
      run(0, input)
  }

  override def part1: Int = // 13087969
    new IntcodeComputer(initialState).run(1)

  override def part2: Int = // 14110739
    new IntcodeComputer(initialState).run(5)
}
