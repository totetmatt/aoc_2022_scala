import scala.io.Source

object Day_04 extends App {
  val filename = "input/day_04/input"
  def common = Source.fromFile(filename)
    .getLines
    .map(_.split(",").map(_.split("-").map(_.toInt).toSeq).toSeq)
    .map(l => (l.head, l.last))
    .map { case (a, b) =>
      val sa = a.head.to(a.last).toSet
      val sb = b.head.to(b.last).toSet
      val u = sa.union(sb)
      (sa, sb, u)
    }

  val p1 = common.count { case (a, b, u) => a.size == u.size || b.size == u.size }
  println(p1)

  val p2 = common.count { case (a, b, u) => u.size < a.size + b.size }
  println(p2)


}
