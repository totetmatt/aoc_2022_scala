
import scala.io.Source

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

  override def toString(): String = {
    s"(${x},${y})"
  }
}

object Day_09 extends App {
  val filename = "input/day_09/input"

  def solution(size: Int) = {
    val steps = Source.fromFile(filename)
      .getLines
      .map(x => {
        val q = x.split(" ")
        (q.head.head, q.tail.head.toInt)
      })
      .flatMap { case (dir, steps) => (0 until steps).map(_ => dir) }
      .map {
        case 'R' => vec2(1, 0)
        case 'L' => vec2(-1, 0)
        case 'U' => vec2(0, 1)
        case 'D' => vec2(0, -1)
      }.foldLeft(
      (Seq.fill(size)(vec2(0, 0)), Set(vec2(0, 0))
      ))(
      (acc, dir) => {
        val Tuple2(rope, visited) = acc
        val newHead = rope.head + dir

        val newRope = rope.tail.foldLeft(Seq(newHead))((r, t) => {
          val nt = r.last - t match {
            case v if v.x.abs >= 2 || v.y.abs >= 2 =>  t + v.normalize()
            case _ => t
          }
          r :+ nt

        }
        )


        (newRope, visited + newRope.last)

      }

    )
    println(steps._2.size)
  }

  solution(2)
  solution(10)
}