import scala.io.Source
object Day_06 extends App {Seq(4,14).foreach(x=>println(Source.fromFile("input/day_06/input").mkString.sliding(x).indexWhere(_.toSet.size==x)+x))}
