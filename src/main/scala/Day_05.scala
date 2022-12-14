import scala.collection.mutable
import scala.io.Source

object Day_05 extends App {
  val filename = "input/day_05/input"

  {
    val warehouse: mutable.Map[Int, mutable.Seq[Char]] = mutable.Map.empty[Int, mutable.Seq[Char]]
    warehouse.withDefaultValue(mutable.Seq.empty[Char])

    Source.fromFile(filename)
      .getLines
      .foreach {
        case s"move $move from $from to $to" =>
          val (m, s) = warehouse.get(from.toInt).map(_.splitAt(move.toInt)).get
          warehouse.put(from.toInt, s)
          warehouse.put(to.toInt, m.reverse ++ warehouse(to.toInt))
        case l if l.contains("[") => l.zipWithIndex
          .filter(_._1.isLetter)
          .map { case (c, i) => (c, 1 + (i - 1) / 4) }
          .foreach { case (c, i) => warehouse.put(i, warehouse.getOrElse(i, mutable.Seq.empty).appended(c)) }
        case _ =>
      }

    println(
      warehouse.keys.toSeq.sorted.map(k => warehouse(k).head).mkString("")
    )
  }

  {
    val warehouse: mutable.Map[Int, mutable.Seq[Char]] = mutable.Map.empty[Int, mutable.Seq[Char]]
    warehouse.withDefaultValue(mutable.Seq.empty[Char])

    Source.fromFile(filename)
      .getLines
      .foreach {
        case s"move $move from $from to $to" =>
          val (m, s) = warehouse.get(from.toInt).map(_.splitAt(move.toInt)).get
          warehouse.put(from.toInt, s)
          warehouse.put(to.toInt, m ++ warehouse(to.toInt))
        case l if l.contains("[") => l.zipWithIndex
          .filter(_._1.isLetter)
          .map { case (c, i) => (c, 1 + (i - 1) / 4) }
          .foreach { case (c, i) => warehouse.put(i, warehouse.getOrElse(i, mutable.Seq.empty).appended(c)) }
        case _ =>

      }

    println(
      warehouse.keys.toSeq.sorted.map(k => warehouse(k).head).mkString("")
    )
  }

}
