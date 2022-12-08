
import scala.collection.mutable
import scala.io.Source

object Day_08 extends App {
  val filename = "input/day_08/input"
  // Y X coord
  val theMap = Source.fromFile(filename)
    .getLines
    .map(_.toSeq.map(_.toString.toInt).zipWithIndex)
    .zipWithIndex
    .toSeq

  val width = theMap.head._1.size


  { // Part 1
    val lineScan = theMap.flatMap { case (line, y) =>
      line.foldLeft(Seq(line.head))((acc, el) => if (acc.last._1 < el._1) acc :+ el else acc).toSet
        .union(
          line.foldRight(Seq(line.last))((el, acc) => if (acc.last._1 < el._1) acc :+ el else acc).toSet
        ).map { case (_, x) => (y, x) }.toSeq.sorted

    }
    val columnScan = (0 until width)
      .map(x => (
        theMap
          .map(_._1(x)._1).zipWithIndex, x)
      )
      .flatMap { case (line, x) =>
        line.foldLeft(Seq(line.head))((acc, el) => if (acc.last._1 < el._1) acc :+ el else acc).toSet
          .union(
            line.foldRight(Seq(line.last))((el, acc) => if (acc.last._1 < el._1) acc :+ el else acc).toSet
          ).map { case (_, y) => (y, x) }

      }
    println(lineScan.toSet.union(columnScan.toSet).size)
  }
  { // Part 2
    val lineScan = theMap.flatMap { case (line, y) =>
      line.map { case (sapin, x) =>
        val (l, r) = line.splitAt(x)

        val rSpan = r.tail.span(_._1 < sapin)
        val rScore = rSpan._1.size + {
          if (rSpan._2.isEmpty) 0 else 1
        }
        val lSpan = l.reverse.span(_._1 < sapin)
        val lScore = lSpan._1.size + {
          if (lSpan._2.isEmpty) 0 else 1
        }
        ((y, x), lScore * rScore)
      }


    }

    val columnScan = (0 until width)
      .map(x => (
        theMap
          .map(_._1(x)._1).zipWithIndex, x)
      )
      .flatMap { case (col, x) =>
        col.map { case (sapin, y) =>
          val (l, r) = col.splitAt(y)
          val rSpan = r.tail.span(_._1 < sapin)
          val rScore = rSpan._1.size + {
            if (rSpan._2.isEmpty) 0 else 1
          }
          val lSpan = l.reverse.span(_._1 < sapin)
          val lScore = lSpan._1.size + {
            if (lSpan._2.isEmpty) 0 else 1
          }
          ((y, x), lScore * rScore)
        }
      }

    println(
      (lineScan ++ columnScan)
        .groupBy(_._1)
        .map { case (_, v) => v.map(_._2).product }
        .max
    )

  }


}


