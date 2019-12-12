object Day10 extends Base(10) {

  private val map = inputLines
  private val width = map.head.length
  private val height = map.length

  private def asteroidsPositions =
    (0 until height)
      .flatMap(y => (0 until width).map((_, y)))
      .filter { case (x, y) => map(y)(x) == '#' }

  private def detectableAsteroids(station: (Int, Int)) =
    asteroidsPositions
      .collect {
        case asteroid if asteroid != station =>
          val (_, angle) = polar(station, asteroid)
          angle
      }
      .toSet
      .size

  private def vaporizableAsteroids(station: (Int, Int)) =
    asteroidsPositions
      .collect {
        case asteroid if asteroid != station =>
          val (distance, angle) = polar(station, asteroid)
          angle -> (asteroid, distance)
      }
      .groupKeyValue
      .view
      .flatMap {
        case (angle, asteroids) =>
          asteroids.toSeq.sortBy(_._2).zipWithIndex.map {
            case ((asteroid, _), ix) =>
              angle + ix * 2D * math.Pi -> asteroid
          }
      }
      .toSeq.sortBy(_._1).view
      .map(_._2)
      .toSeq

  private def polar(station: (Int, Int), asteroid: (Int, Int)) = {
    val (x1, y1) = station
    val (x2, y2) = asteroid
    val dx = x2.toDouble - x1
    val dy = y2.toDouble - y1
    val distance = math.sqrt(dx * dx + dy * dy)
    val angle = (math.Pi - math.atan2(dx, dy)) % (2D * math.Pi)
    (distance, angle)
  }

  val monitoringStation = asteroidsPositions.maxBy(detectableAsteroids)
  println(monitoringStation)

  override def part1: Int = // 214
    detectableAsteroids(monitoringStation)

  override def part2 = //
    vaporizableAsteroids(monitoringStation)
      .drop(199)
      .map { case (x, y) => 100 * x + y }
      .head
}
