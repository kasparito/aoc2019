object Day4 extends Base(4) {

  val passwordRange: Seq[String] = (272091 to 815432).map(_.toString)

  def adjacentDigits(password: String): Boolean =
    password
      .iterator
      .sliding(2)
      .exists { case Seq(x, y) => x == y }

  def neverDecrease(password: String): Boolean =
    password
      .iterator
      .sliding(2)
      .forall { case Seq(x, y) => x <= y }

  override def part1: Int = // 931
    passwordRange.count(p => adjacentDigits(p) & neverDecrease(p))

  def soloAdjacentDigits(password: String): Boolean =
    (" " + password + " ")
      .iterator
      .sliding(4)
      .exists { case Seq(pre, x, y, post) => pre != x && x == y && y != post }

  override def part2: Int = // 609
    passwordRange.count(p => soloAdjacentDigits(p) & neverDecrease(p))
}
