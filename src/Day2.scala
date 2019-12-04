object Day2 extends Base(2) {

  def initialState: Array[Int] = inputLines.head.split(',').map(_.toInt)

  def run(state: Array[Int], noun: Int, verb: Int): Int = {

    @scala.annotation.tailrec
    def run(position: Int): Int =
      state.slice(position, position + 4) match {
        case array if array.head == 99 =>
          state(0)
        case Array(operation, term1, term2, result) =>
          val operationFunction: (Int, Int) => Int = operation match {
            case 1 => _ + _
            case 2 => _ * _
          }
          state(result) = operationFunction(state(term1), state(term2))
          run(position + 4)
      }

    state(1) = noun
    state(2) = verb

    run(0)
  }

  override def part1: Int = // 3850704
    run(initialState, 12, 2)

  override def part2: String = { // 6718
    for {
      noun <- 0 to 99
      verb <- 0 to 99
      result = run(initialState, noun, verb)
      if result == 19690720
    } yield s"$noun$verb"
  }.head
}
