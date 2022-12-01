import scala.io.Source

object Day_01 extends App {
  val filename = "input/day_01/input"
  val q = Source.fromFile(filename)
    .getLines
    .foldLeft(
      (0, Seq.fill(3)(0))
    )((acc, el) => {
      el.toIntOption match {
        case Some(v) => acc.copy(_1 = acc._1 + v)
        case _ => (0, acc._2 match {
          case h :: m :: _ if acc._2.head < acc._1 => Seq(acc._1, h, m)
          case h :: m :: _ if acc._2(1) < acc._1 => Seq(h, acc._1, m)
          case h :: m :: _ if acc._2(2) < acc._1 => Seq(h, m, acc._1)
          case _ => acc._2
        })
      }
    })
  println(q._2.head)
  println(q._2.sum)
}
