object Day5 extends Base(5) {

  def initialState: Array[Int] = inputLines.head.split(',').map(_.toInt)

  def run(state: Array[Int], input: Int): Int = {

    @scala.annotation.tailrec
    def run(position: Int, input: Int): Int = {
      val operation = state(position).toString
      operation.takeRight(2).toInt match {
        case 1 =>
          val Array(term1, term2, result) = state.slice(position + 1, position + 4)
          val Seq(termType1, termType2) = operation.dropRight(2).reverse.map(_.toString.toInt).padTo(2, 0)
          def term(v: Int, t: Int) = if (t == 1) v else state(v)
          state(result) = term(term1, termType1) + term(term2, termType2)
          run(position + 4, input)
        case 2 =>
          val Array(term1, term2, result) = state.slice(position + 1, position + 4)
          val Seq(termType1, termType2) = operation.dropRight(2).reverse.map(_.toString.toInt).padTo(2, 0)
          def term(v: Int, t: Int) = if (t == 1) v else state(v)
          state(result) = term(term1, termType1) * term(term2, termType2)
          run(position + 4, input)
        case 3 =>
          val result = state(position + 1)
          state(result) = input
          run(position + 2, input)
        case 4 =>
          val result = state(position + 1)
          run(position + 2, state(result))
        case 5 =>
          val Array(term1, term2) = state.slice(position + 1, position + 3)
          val Seq(termType1, termType2) = operation.dropRight(2).reverse.map(_.toString.toInt).padTo(2, 0)
          def term(v: Int, t: Int) = if (t == 1) v else state(v)
          val newPosition =
            if (term(term1, termType1) != 0)
              term(term2, termType2)
            else
              position + 3
          run(newPosition, input)
        case 6 =>
          val Array(term1, term2) = state.slice(position + 1, position + 3)
          val Seq(termType1, termType2) = operation.dropRight(2).reverse.map(_.toString.toInt).padTo(2, 0)
          def term(v: Int, t: Int) = if (t == 1) v else state(v)
          val newPosition =
            if (term(term1, termType1) == 0)
              term(term2, termType2)
            else
              position + 3
          run(newPosition, input)
        case 7 =>
          val Array(term1, term2, result) = state.slice(position + 1, position + 4)
          val Seq(termType1, termType2) = operation.dropRight(2).reverse.map(_.toString.toInt).padTo(2, 0)
          def term(v: Int, t: Int) = if (t == 1) v else state(v)
          state(result) = if (term(term1, termType1) < term(term2, termType2)) 1 else 0
          run(position + 4, input)
        case 8 =>
          val Array(term1, term2, result) = state.slice(position + 1, position + 4)
          val Seq(termType1, termType2) = operation.dropRight(2).reverse.map(_.toString.toInt).padTo(2, 0)
          def term(v: Int, t: Int) = if (t == 1) v else state(v)
          state(result) = if (term(term1, termType1) == term(term2, termType2)) 1 else 0
          run(position + 4, input)
        case 99 =>
          input
      }
    }

    run(0, input)
  }

  override def part1: Int = // 13087969
    run(initialState, 1)

  override def part2: Int = // 14110739
    run(initialState, 5)
}
