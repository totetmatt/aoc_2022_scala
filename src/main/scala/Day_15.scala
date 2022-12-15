import sun.misc.Signal

case class Vec2(x:Int,y:Int) {
  def -(other:Vec2) = {
    Vec2(x-other.x,y-other.y)
  }

  def mdist(other:Vec2) = {
      val v = this - other
      (v.x.abs+v.y.abs)
  }

}


trait Radio {
  val v:Vec2
}
case class Sensor(range:Integer,v:Vec2) extends Radio {}
case class Beacon(v:Vec2) extends  Radio
object Day_15 extends App {
  val yCheck = 2000000
  val iFile = "input/day_15/input"

  val map: Seq[Radio] = scala.io.Source.fromFile(iFile)
    .getLines
    .flatMap {
      case s"Sensor at x=${sx}, y=${sy}: closest beacon is at x=${bx}, y=${by}" =>

        val b = Vec2(bx.toInt, by.toInt)
        val s = Vec2(sx.toInt, sy.toInt)
        // println(  Seq( Beacon(b), Sensor(s.mdist(b),s)))
        Seq( Beacon(b), Sensor(s.mdist(b),s))
    }.toSeq

  val allX= map.map(_.v.x)
  val allY = map.map(_.v.y)
  val maxRange = map.map{
    case Sensor(l,_) => l
    case _ => 0
  }.asInstanceOf[Seq[Int]].max
  val topLeft = Vec2( allX.min-maxRange, allY.min)
  val bottomRight = Vec2( allX.max+maxRange, allY.max)



 // Extremly unoptimized
  val t = (topLeft.x to bottomRight.x).map(x=>
    Vec2(x,yCheck)
  )
    .filter(v=>
      map.exists {

        case Sensor(r, sv) => sv.mdist(v) <= r
        case _: Radio => false
      }
    )
    .filterNot(v=>
      map.exists {
        case Beacon(bv) => bv == v
        case _: Radio => false
      }
    )
  println("Part1", t.size)

  val sensors = map.filter {
    case Sensor(_, _) => true
    case _ => false
  }.sortBy(_.v.x).asInstanceOf[Seq[Sensor]]
  val limit = 4000000
  for (y <- 0 to limit) {
    val l = sensors.flatMap(s => {
      val pr = s.range - (y - s.v.y).abs
      if (pr >= 0) Some(Math.max(0, s.v.x - pr), Math.min(s.v.x + pr, limit)) else None
    }
    ).sortBy(_._1)
      .reduce(
        (acc, el) => {
          if (el._1 - 1 <= acc._2) acc.copy(_2 = Math.max(acc._2, el._2)) else acc
        }
      )
    if (l._2 != limit) {
      println("x:", l._2 + 1, "y:", y)
      println("Part 2:", BigInt(l._2 + 1) * BigInt(4000000) + BigInt(y))
    }
  }


}
