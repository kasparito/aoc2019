import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait Handler[T] {
  def input: Long
  def handle(output: Long): Unit
  def output: T
}

class SimpleHandler(initialInput: Seq[Long] = Seq.empty) extends Handler[Seq[Long]] {
  private var remainingInput = initialInput
  private var intermediateOutput = Seq.empty[Long]

  override def input: Long = {
    val next = remainingInput.head
    remainingInput = remainingInput.tail
    next
  }

  override def handle(output: Long): Unit =
    intermediateOutput = intermediateOutput :+ output

  override def output: Seq[Long] =
    intermediateOutput
}

object IntcodeComputer {
  def run[T](initialState: Array[Long], handler: Handler[T]): Future[T] =
    Future(new IntcodeComputer(initialState, handler.input, handler.handle).run())
      .map(_ => handler.output)
}

class IntcodeComputer(
    initialState: Array[Long],
    input: => Long,
    output: Long => Unit) {

  private val stateMap = mutable.Map[Long, Long]() ++ initialState.zipWithIndex.map { case (a, b) => (b.toLong, a) }
  private def state(ix: Long): Long = stateMap.getOrElseUpdate(ix, 0L)

  private var position = 0L
  private var relativeBase = 0L

  def run(): Unit = while (true) {
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
      val value = state(position + i)
      val resultPosition = termTypes(i - 1) match {
        case 0 => value
        case 2 => value + relativeBase
      }
      stateMap(resultPosition) = v
    }

    operation match {
      case 1 =>
        set(3, term(1) + term(2))
        position += 4
      case 2 =>
        set(3, term(1) * term(2))
        position += 4
      case 3 =>
        set(1, input)
        position += 2
      case 4 =>
        val t = term(1)
        position += 2
        output(t)
      case 5 =>
        position = if (term(1) != 0) term(2) else position + 3
      case 6 =>
        position = if (term(1) == 0) term(2) else position + 3
      case 7 =>
        set(3, if (term(1) < term(2)) 1 else 0)
        position += 4
      case 8 =>
        set(3, if (term(1) == term(2)) 1 else 0)
        position += 4
      case 9 =>
        relativeBase += term(1)
        position += 2
      case 99 =>
        return
    }
  }
}
