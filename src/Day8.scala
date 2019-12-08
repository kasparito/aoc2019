object Day8 extends Base(8) {

  private val width = 25
  private val height = 6
  private val pixelCount = width * height

  override def part1: Int = { // 2176
    val row = inputLines.head.iterator.grouped(pixelCount).minBy(_.count(_ == '0'))
    row.count(_ == '1') * row.count(_ == '2')
  }

  override def part2: String = { // CYKBY
    val layers = inputLines.head.iterator.grouped(pixelCount).toSeq
    val pixels = (0 until pixelCount).map(ix => layers.map(_(ix)).find(_ != '2').get)
    pixels
      .grouped(width)
      .map(_.mkString
        .replace('0', ' ')
        .replace('1', '#'))
      .mkString("\n", "\n", "\n")
  }
}
