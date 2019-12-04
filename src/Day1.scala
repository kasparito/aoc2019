object Day1 extends Base(1) {

  val nums = inputLines.map(_.toInt)

  override def part1: Int =
    nums.map(fuel).sum

  def fuel(i: Int): Int =
    (math.floor(i.toDouble / 3) - 2).toInt

  override def part2: Int =
    nums.map(fuelFuel).sum

  def fuelFuel(i: Int): Int = {
    val f = fuel(i)
    if (f <= 0)
      0
    else
      f + fuelFuel(f)
  }
}
