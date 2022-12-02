import scala.io.Source

object Day_02 extends App {

  val filename = "input/day_02/input"
  val part1 = Source.fromFile(filename)
    .getLines
    .map(part_1)
    .sum

  /*
  *  X => Loose 0
  *  y => Draw  3
  *  Z => Win   6
  * */
  val part2 = Source.fromFile(filename)
    .getLines
    .map(part_2)
    .sum

  def part_1(move: String): Int = {
    val Array(opponent, me) = move.split(" ")
    val op = "ABCABCABC"
    val my = "XYZXYZXYZ"

    (op.indexOf(opponent, 3) -
      my.indexOf(me, 3) match {
      case -1 | 2 => 6
      case 0 => 3
      case 1 | -2 => 0
    }) + my.indexOf(me) + 1
  }
  println(part1)

  def part_2(move: String): Int = {
    val Array(opponent, result) = move.split(" ")
    val r = "ABCABCABC"
    val (idx, score) = result match {
      case "X" => (-1, 0)
      case "Y" => (0, 3)
      case "Z" => (1, 6)
    }
    score + r.indexOf(r(r.indexOf(opponent) + 3 + idx).toString) + 1
  }

  println(part2)


}
