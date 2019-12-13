import scala.collection.mutable

object Day11 extends Base(11) {

  def initialState: Array[Long] = inputLines.head.split(',').map(_.toLong)

  trait Heading {
    def move(p: (Int, Int)): (Int, Int)
    def turn(d: Long): Heading
  }
  object Up extends Heading {
    override def move(p: (Int, Int)): (Int, Int) = (p._1, p._2 + 1)
    override def turn(d: Long): Heading = if (d == 0) Left else Right
  }
  object Down extends Heading {
    override def move(p: (Int, Int)): (Int, Int) = (p._1, p._2 - 1)
    override def turn(d: Long): Heading = if (d == 0) Right else Left
  }
  object Left extends Heading {
    override def move(p: (Int, Int)): (Int, Int) = (p._1 - 1, p._2)
    override def turn(d: Long): Heading = if (d == 0) Down else Up
  }
  object Right extends Heading {
    override def move(p: (Int, Int)): (Int, Int) = (p._1 + 1, p._2)
    override def turn(d: Long): Heading = if (d == 0) Up else Down
  }

  override def part1: Int = // 2082
    IntcodeComputer.run(initialState, new Handler[Int] {

      val map: mutable.Map[(Int, Int), Long] = mutable.Map()
      var position: (Int, Int) = (0, 0)
      var heading: Heading = Up

      var handler: Long => Unit = paintColor

      def paintColor(color: Long): Unit = {
        map.put(position, color)
        handler = turn
      }

      def turn(direction: Long): Unit = {
        heading = heading.turn(direction)
        position = heading.move(position)
        handler = paintColor
      }

      override def input: Long = map.getOrElseUpdate(position, 0)

      override def handle(output: Long): Unit = handler(output)

      override def output: Int = map.size
    }).await

  override def part2: String = // FARBCFJK
    IntcodeComputer.run(initialState, new Handler[String] {

      val map: mutable.Set[(Int, Int)] = mutable.Set((0,0))
      var position: (Int, Int) = (0, 0)
      var heading: Heading = Up

      var handler: Long => Unit = paintColor

      def paintColor(color: Long): Unit = {
        if (color == 1)
          map.add(position)
        else
          map.remove(position)
        handler = turn
      }

      def turn(direction: Long): Unit = {
        heading = heading.turn(direction)
        position = heading.move(position)
        handler = paintColor
      }

      override def input: Long = if (map.contains(position)) 1 else 0

      override def handle(output: Long): Unit = handler(output)

      override def output: String = {
        val xMin = map.view.map(_._1).min
        val xMax = map.view.map(_._1).max
        val yMin = map.view.map(_._2).min
        val yMax = map.view.map(_._2).max
        (yMin to yMax).map {
          y =>
            (xMin to xMax).collect {
              case x if map.contains((x, y)) => '#'
              case _ => ' '
            }.mkString
        }.reverse.mkString("\n", "\n", "")
      }
    }).await
}
