import scala.annotation.tailrec
import scala.io.AnsiColor._
object Day_12 extends App {
  case class Tile(c:Char,v:vec2, s:Option[Int]=None) {
    def isCandidate(other:Tile): Boolean = {
      v.isAdjacent(other.v) &&  (c match {
        case 'S' => other.c == 'a'
        case 'z' | 'y' if other.c == 'E' => true
        case cc  if other.c != 'E'  =>  cc+1 >=other.c
        case cc => false
      })
    }
  }
  case class vec2(x:Int,y:Int) {
    def isAdjacent(other:vec2): Boolean = {
      ((x-other.x).abs ==1 &&  (y-other.y).abs ==0 )||
        ((y-other.y).abs ==1 &&  (x-other.x).abs ==0)
    }
    def -(other:vec2): vec2 = {
      vec2(x-other.x,y-other.y)
    }
    def dot(other:vec2): Int = {
      x*other.x + y*other.y
    }
  }

  val carte = collection.mutable.HashMap.from(scala.io.Source.fromFile("input/day_12/input")
    .getLines
    .zipWithIndex
    .flatMap{
      case (l,y) => l.zipWithIndex.map{
        case ('S',x) => vec2(x,y) -> Tile('S',vec2(x,y),s=Some(0))
        case (c,x) => vec2(x,y) -> Tile(c,vec2(x,y))
      }
    }.toMap)
  val carte2 = carte.clone()
  def printCarte() = {
    val width = carte.keys.map(_.x).max
    val height = carte.keys.map(_.y).max
    (0 to height).foreach(y => {
      (0 to width).foreach(x => {
        val p = carte(vec2(x, y))
        p.s match {
          case Some(c) => print(s"${RED}${p.c}${RESET}")
          case _ => print(p.c)
        }
      })
      println()
    }
    )
  }

  {
    val start = carte.find(_._2.c == 'S').get._2
    val end = carte.find(_._2.c == 'E').get._2
    val toVisit = collection.mutable.ArrayBuffer(start)

    while (toVisit.nonEmpty) {
      val current = toVisit.remove(0)

      carte(current.v) = current
      val candidates = carte
        .values
        .filter(
          n => current.isCandidate(n)
            && !toVisit.map(_.v).contains(n.v)
            && (carte(n.v).s.isEmpty)
        )
        .map(n => n.copy(s = Some(current.s.get + 1))).toSeq.sortBy(n => n.v.dot(end.v))


      toVisit.prependAll(candidates)
      toVisit.sortInPlaceBy(_.s)


    }


    printCarte()
    println("part 1", carte.find(_._2.c == 'E'))
  }



  // PART 2
  /**
   * If I look at the map, the b's are only on the vec2(1,Y) which means that only a's around this point are valid.
   * So we just need to check what are the distance for all the a on the vec2(0,Y)
   * Is it fair ? I don't know . Is it stupid ? Yes but it works
   */
  {
    val ys = carte2.keys.map(_.y).max
    (0 to ys).foreach { y=>
      val carteo = carte2.clone()
      val start = carteo(vec2(0,y))
      carteo(vec2(0,y)) = start.copy(s=Some(0))
      val end = carteo.find(_._2.c == 'E').get._2
      val toVisit = collection.mutable.ArrayBuffer( carteo(vec2(0,y)) )
      while (toVisit.nonEmpty) {
        val current = toVisit.remove(0)
        carteo(current.v) = current
        val candidates = carteo
          .values
          .filter(
            n => current.isCandidate(n)
              && !toVisit.map(_.v).contains(n.v)
              && (carteo(n.v).s.isEmpty)
          )
          .map(n => n.copy(s = Some(current.s.get + 1))).toSeq.sortBy(n => n.v.dot(end.v))
        toVisit.prependAll(candidates)
        toVisit.sortInPlaceBy(_.s)

      }

      println(carteo.find(_._2.c == 'E'))
    }
  }

}
