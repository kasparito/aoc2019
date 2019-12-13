object Day12 extends Base(12) {

  val inputLines2: List[String] =
    """<x=-8, y=-10, z=0>
      |<x=5, y=5, z=10>
      |<x=2, y=-7, z=3>
      |<x=9, y=-8, z=-3>""".stripMargin.split('\n').toList

  private val MoonPattern = """<x=([^,>\s]+), y=([^,>\s]+), z=([^,>\s]+)>""".r

  private def initialMoons: Seq[Moon] = inputLines.map {
    case MoonPattern(x, y, z) =>
      new Moon(new Vector(x.toInt, y.toInt, z.toInt), new Vector(0, 0, 0))
  }

  class Vector(var x: Int, var y: Int, var z: Int) {

    def add(vector: Vector): Unit = {
      x += vector.x
      y += vector.y
      z += vector.z
    }

    def energy: Int =
      math.abs(x) + math.abs(y) + math.abs(z)

    override def equals(obj: Any): Boolean =
      obj match {
        case that: Vector =>
          this.x == that.x &&
            this.y == that.y &&
            this.z == that.z
        case _ =>
          false
      }

    override def toString: String = s"$x,$y,$z"
  }

  class Moon(val position: Vector, val velocity: Vector) {

    def addGravityEffect(moon: Moon): Unit =
      if (moon != this) {
        velocity.x += Integer.compare(moon.position.x, position.x)
        velocity.y += Integer.compare(moon.position.y, position.y)
        velocity.z += Integer.compare(moon.position.z, position.z)
      }

    def move(): Unit =
      position.add(velocity)

    def totalEnergy: Int = {
      val potentialEnergy = position.energy
      val kineticEnergy = velocity.energy
      potentialEnergy * kineticEnergy
    }

    override def equals(obj: Any): Boolean =
      obj match {
        case that: Moon =>
          this.position == that.position &&
            this.velocity == that.velocity
        case _ =>
          false
      }

    override def toString: String = s"$position;$velocity"
  }

  private def move(moons: Seq[Moon]): Unit = {
    moons.foreach(moon => moons.foreach(moon.addGravityEffect))
    moons.foreach(_.move())
  }

  override def part1: Int = { // 9743
    var steps = 0
    val moons = initialMoons
    while (steps < 1000) {
      steps += 1
      move(moons)
    }
    moons.map(_.totalEnergy).sum
  }

  override def part2: Long = { //
    var steps = 0L
    val moons = initialMoons
    val initial = initialMoons
    do {
      steps += 1
      if (steps % 10000000 == 0) println(steps)
      move(moons)
    } while (moons != initial)
    steps
  }
}
