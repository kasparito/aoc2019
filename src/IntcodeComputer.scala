import scala.collection.mutable

trait State {
  def output: Seq[Long]
}
case class WaitingForInput(output: Seq[Long]) extends State
case class Halted(output: Seq[Long]) extends State

class IntcodeComputer(initialState: Array[Long]) {

  private val stateMap = mutable.Map[Long, Long]() ++ initialState.zipWithIndex.map { case (a, b) => (b.toLong, a) }
  private def state(ix: Long): Long = stateMap.getOrElseUpdate(ix, 0L)

  private var position = 0L
  private var relativeBase = 0L

  @scala.annotation.tailrec
  private def run(input: Seq[Long], output: Seq[Long]): State = {
    val instruction = state(position).toString
    val operation = instruction.takeRight(2).toInt
    val termTypes = instruction.dropRight(2).reverse.map(_.toString.toInt).padTo(3, 0)

    def term(i: Int): Long = {
      val value = state(position + i)
      termTypes(i - 1) match {
        case 0 => state(value)
        case 1 => value
        case 2 => state(value + relativeBase)
      }
    }

    def set(i: Int, v: Long): Unit = {
      val resultPosition = termTypes(i - 1) match {
        case 0 => position + i
        case 2 => position + i + relativeBase
      }
      stateMap(state(resultPosition)) = v
    }

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
      case 9 =>
        relativeBase += term(1)
        position += 2
        run(input, output)
      case 99 =>
        Halted(output)
    }
  }

  def run(input: Seq[Long]): State =
    run(input, Seq.empty)
}
