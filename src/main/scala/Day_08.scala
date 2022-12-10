
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

    // Returns the Set of visible tree on left and right
    def computeSet(map:Seq[(Seq[(Int,Int)],Int)]) = {
        map.flatMap { case (line, y) =>
          (
              line.foldLeft (Seq(line.head))((acc, el) => if (acc.last._1 < el._1) acc :+ el else acc).toSet ++
              line.foldRight(Seq(line.last))((el, acc) => if (acc.last._1 < el._1) acc :+ el else acc).toSet
            )
            .map { case (_, x) => (y, x) }
            .toSeq

        }.toSet
    }


    val part1 = (
                    computeSet(theMap) ++ // lineScan
                    computeSet((0 until width).map(x => (theMap.map(_._1(x)._1).zipWithIndex, x))) // columnScan = Map rotated to have scanLine
                )
                .size
    println(part1)
  }
  { // Part 2


    def distance(line:(Seq[(Int,Int)],Seq[(Int,Int)])) = {
      line._1.size + {
        if (line._2.isEmpty) 0 else 1
      }
    }

    val lineScan = theMap.flatMap { case (line, y) =>
      line.map { case (sapin, x) =>
        val (l, r) = line.splitAt(x)
        val rScore =distance(r.tail.span(_._1 < sapin))
        val lScore =distance(l.reverse.span(_._1 < sapin))
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
          val rScore = distance(r.tail.span(_._1 < sapin))
          val lScore = distance(l.reverse.span(_._1 < sapin))
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


