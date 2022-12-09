
import scala.io.Source

object Day_09 extends App {
  val filename = "input/day_09/test"
  val steps = Source.fromFile(filename)
    .getLines
    .map(
      _.split(' ').map(x => x.head).toSeq
    )
    .map(x => (x.head, x.last.toString.toInt))
    .flatMap { case (dir, steps) => (0 until steps).map(_ => dir) }
    .map {
      case 'R' => ('R', vec2(1, 0))
      case 'L' => ('L', vec2(-1, 0))
      case 'U' => ('U', vec2(0, 1))
      case 'D' => ('D', vec2(0, -1))
    }
    .foldLeft(Rope(vec2(0, 0), Seq(vec2(0, 0))))(
      (acc, step) => {
        val (c, dir) = step
        val newState = acc.applyTension(acc.head + dir)
        println(acc.head, "\t", acc.tail.last, "\t", "--", c, "-->", "\t", newState.head, "\t", newState.tail.last)


        newState

      })

  //X Y  coord
  case class vec2(x: Int, y: Int) {
    def +(other: vec2): vec2 = {
      vec2(this.x + other.x, this.y + other.y)
    }

    def -(other: vec2): vec2 = {
      vec2(this.x - other.x, this.y - other.y)
    }

    def normalize(): vec2 = {
      vec2(this.x.sign, this.y.sign)
    }
  }
  // more than 4562 4748

  case class Rope(head: vec2, tail: Seq[vec2]) {
    def applyTension(newHead: vec2): Rope = {

      (newHead - tail.last) match {

        case v if v.y.abs >= 2 || v.x.abs >= 2 => {
          val t = tail.last + v.normalize()
          Rope(newHead, tail :+ t)
        }
        case _ => Rope(newHead, tail)
      }
    }
  }

  println(steps.tail.toSet.size)


}


