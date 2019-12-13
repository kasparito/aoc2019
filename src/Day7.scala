import java.util.Collections
import java.util.concurrent.{BlockingQueue, LinkedBlockingDeque}
import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Day7 extends Base(7) {

  def initialState: Array[Long] = inputLines.head.split(',').map(_.toLong)

  override def part1: Long = // 880726
    (0 to 4).permutations.map(run).max

  override def part2: Long = // 4931744
    (5 to 9).permutations.map(run).max

  private def run(settings: Seq[Int]): Long = {
    val inputQueues = settings
      .map(phaseSetting => new LinkedBlockingDeque[Long](Collections.singleton(phaseSetting.toLong)))
      .toIndexedSeq
    inputQueues.head.put(0)
    Future.sequence(inputQueues.zipWithIndex.map {
      case (inputQueue, index) =>
        val outputQueue = inputQueues((index + 1) % inputQueues.size)
        IntcodeComputer.run(initialState, new PhaseHandler(inputQueue, outputQueue))
    }).await.last
  }

  class PhaseHandler(
      inputQueue: BlockingQueue[Long],
      outputQueue: BlockingQueue[Long])
    extends Handler[Long] {

    private val lastOutput = new AtomicLong()

    override def input: Long = inputQueue.take()

    override def handle(output: Long): Unit = {
      lastOutput.set(output)
      outputQueue.put(output)
    }

    override def output: Long = lastOutput.get()
  }
}
