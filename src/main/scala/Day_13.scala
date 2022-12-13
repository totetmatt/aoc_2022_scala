object Day_13 extends App {

  val input = scala.io.Source.fromFile("input/day_13/input")
    .getLines.toSeq

  trait Elem
  case class Value(e:String) extends Elem
  case class SubList(e:String) extends Elem
  def parse(str:String) : Seq[Elem] = {
    var out = Seq.empty[Elem]
    var tmp=""
    var p = 0
    for(c <-str.init.tail) {
      c match {
        case '[' => p+=1
          tmp = tmp + c.toString
        case ']' => p-=1
          tmp = tmp + c.toString
        case ',' => if(tmp.nonEmpty && p==0) {
          val v = if(tmp.nonEmpty && tmp.last==']'){
            SubList(tmp)
          } else {
            Value(tmp)
          }
          out = out :+ v
          tmp=""
        } else {
          tmp = tmp + c.toString
        }
        case _ => tmp = tmp + c.toString
      }
    }
    if (tmp.nonEmpty) {
      val v = if (tmp.last == ']') {
        SubList(tmp)
      } else {
        Value(tmp)
      }
      out = out :+ v
    }
      out
  }

  def compare(left:Seq[Elem], right:Seq[Elem]): Option[Boolean] = {
    for( (l,r) <- left.zipAll(right,None,None)) {
      (l,r) match {
        case (Value(a),Value(b)) => if(a.toInt != b.toInt) return Some(a.toInt < b.toInt)
        case (SubList(a),SubList(b)) =>
          val r = compare(parse(a),parse(b))
          if(r.isDefined) return r
        case (Value(a),SubList(b)) =>
          val r = compare(parse(s"[$a]"), parse(b))
          if (r.isDefined) return r

        case (SubList(a), Value(b)) =>
          val r = compare(parse(a), parse(s"[$b]"))
          if (r.isDefined) return r
        case (None, _) => return Some(true)
        case (_, None) => return Some(false)
      }
    }
    None
  }

  val lines = input
    .grouped(3)
    .map(l => (parse(l.head),parse(l.tail.head)))
    .toSeq.zipWithIndex
    .filter{case (x,_) =>compare(x._1,x._2).get }
    .map{case (_,idx) => idx+1 }.sum

  println("Part 1", lines)


  val part2  = (input++ Seq("[[2]]","[[6]]"))
    .filter(_ !="")
    .map(parse).sortWith((a,b) =>compare(a,b).get)
    val a = part2.indexWhere(_ == parse("[[2]]"))+1
  val b = part2.indexWhere(_ == parse("[[6]]")) +1

  println("Part 2", a*b)
}
