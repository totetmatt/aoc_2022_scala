trait Op

case class noop() extends Op
case class addx(value: Int) extends Op

case class State(clock: Int = 0, X: Int = 1) {
  def op(op: Op): Seq[State] = op match {
    case addx(v) => Seq(State(clock + 1, X), State(clock + 2, X + v))
    case _ => Seq(State(clock + 1, X))
  }
  override def toString: String =  s"[${clock}] X=${X}"
}

object Day_10 extends App {
  def sprite(pos: Int = 1) = {
    (0 to 39).map {
      case x: Int if pos + 1 >= x && pos - 1 <= x => "⬜"
      case _ => "⬛"
    }.mkString
  }
  val s = scala.io.Source.fromFile("input/day_10/input")
    .getLines.map(x =>
    x.split(" ").toSeq match {
      case Seq("addx", v) => addx(v.toInt)
      case _ => noop()
    }
  ).foldLeft(Seq(State()))((states, op) => {
    states.appendedAll(states.last.op(op))
  })
  val part1 = (20 to 221 by 40).map(x => x * s(x - 1).X).sum
  val part2 = s.init
    .zipWithIndex
    .map {case (state,idx) => sprite(state.X)(idx%40) }
    .grouped(40)
    .map(_.mkString)
    .mkString("\n")
  println(part1)
  print(part2)
}
