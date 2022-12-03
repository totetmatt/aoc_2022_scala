import scala.io.Source

object Day_03 extends App {

  val prio = (0 to 25).flatMap(i => Seq((('a' + i).toChar, i + 1), (('A' + i).toChar, i + 27))).toMap

  val filename = "input/day_03/input"


  val p1 = Source.fromFile(filename)
    .getLines
    .map(s =>(s.length / 2, s))
    .map(i=> i._2.splitAt(i._1))
    .map{case (x,y) => Seq(x,y)
                      .map(_.toSet)
                      .reduce((a,b)=> a.intersect(b))
                      .head
    }
    .map(prio)
    .sum
  println(p1)

  val p2 = Source.fromFile(filename)
    .getLines
    .grouped(3)
    .map(_.map(_.toSet)
          .reduce(
              (a, b) => a.intersect(b)
          ).head)
    .map(prio)
    .sum
  print(p2)


}
