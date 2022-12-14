object Day_14_2 extends App {

  val ROCK = "█"
  val SAND = "▒"
  val START = "+"
  val AIR = "░"

  case class Vec2(x: Int, y: Int) {

    def minmax(other: Vec2): (Vec2, Vec2) = {
      (
        Vec2(Math.min(x, other.x), Math.min(y, other.y)),
        Vec2(Math.max(x, other.x), Math.max(y, other.y)),
      )
    }
  }

  def print_map(map: collection.mutable.Map[Vec2, String]) = {
    val xx = map.keys.toSeq.sortBy(_.x).map(_.x)
    val yy = map.keys.toSeq.sortBy(_.y).map(_.y)
    (yy.head to yy.last+2).foreach(y => {
      (xx.head to xx.last).foreach(x => {
        if(y==yy.last+2) print(ROCK) else map.getOrElse(Vec2(x, y), AIR).foreach(print)
      })
      println()
    })
    println()
  }

  val input: collection.mutable.Map[Vec2, String] = collection.mutable.Map.from(scala.io.Source.fromFile("input/day_14/test")
    .getLines
    .flatMap(
      _.split("->")
        .map(_.strip().split(",").map(_.strip()).toSeq)
        .map { case Seq(a, b) => Vec2(a.toInt, b.toInt) }
        .sliding(2)
        .flatMap { case Array(a, b) =>

          val (min, max) = a.minmax(b)
          (min.y to max.y).flatMap(y => {
            (min.x to max.x).map(x => {
              Vec2(x, y) -> ROCK
            })
          })
        }
        .toSeq

    ))

  input.put(Vec2(500, 0), START)
  print_map(input)

  val start = Vec2(500, 0)

   {
    val part1Map = input.clone()
    def falldown(start: Vec2, map:collection.mutable.Map[Vec2, String]) : Option[Vec2] = {
      map.filter(_._2 != START).keys.filter(k => k.x == start.x && k.y > start.y) .minByOption(_.y) match {
        case Some(bedrock) if !map.keys.exists(v => v.x == bedrock.x - 1 && v.y == bedrock.y) => falldown(bedrock.copy(x=bedrock.x-1),map)
        case Some(bedrock) if !map.keys.exists(v => v.x == bedrock.x + 1 && v.y == bedrock.y) => falldown(bedrock.copy(x=bedrock.x+1),map)
        case Some(bedrock) => Some(bedrock.copy(y=bedrock.y-1))
        case _ => None
      }
    }
    var r = falldown(start,part1Map)
    var i = 0
    while(r.isDefined){
      val v = r.get
      part1Map.put(v,SAND)

      r = falldown(start,part1Map)
      i+=1
    }
    println(i)
   }

  {
    val part2Map = input.clone()


    def falldown(start: Vec2, map: collection.mutable.Map[Vec2, String], maxY:Int): Option[Vec2] = {

      map.filter(_._2 != START).keys.filter(k => k.x == start.x && k.y > start.y).minByOption(_.y) match {

        case Some(bedrock) if !map.keys.exists(v => v.x == bedrock.x - 1 && v.y == bedrock.y) => falldown(bedrock.copy(x = bedrock.x - 1), map,maxY)
        case Some(bedrock) if !map.keys.exists(v => v.x == bedrock.x + 1 && v.y == bedrock.y) => falldown(bedrock.copy(x = bedrock.x + 1), map,maxY)
        case Some(bedrock) => Some(bedrock.copy(y = bedrock.y - 1))
        case _ => Some(start.copy(y=maxY-1))
      }
    }

    val maxY = part2Map.keys.map(_.y).max +2
    var r = falldown(start, part2Map, maxY)
    var i = 0
    while (!r.contains(start)) {
      val v = r.get


      part2Map.put(v, SAND)

      r = falldown(start, part2Map, maxY)
      i += 1
    }
    println(i+1)
  }


}
