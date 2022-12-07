
import scala.collection.mutable
import scala.io.Source

object Day_07 extends App {
  val filename = "input/day_07/input"

  // Could we use case classes ? Definitively
  val fs = mutable.HashMap.empty[(String, Seq[String]),Int]
  fs.addOne(("d",Seq("/")), 0)
  var context:Seq[String] = Seq("/")
  Source.fromFile(filename)
    .getLines
    .foreach {
      case s"$$ cd $dir" => dir match { // cd
        case "/"  => context = Seq("/")
        case ".." => context = context.init
        case d    => context = context :+ d
      }
      case s"$$ ls" => // ls in our case is a no-op, we wait for outpuut
      case out => out match { // Output from ls
        case s"dir $d" =>fs.addOne((("d",context:+ d) , 0))
        case s"$size $file" => fs.addOne((("f", context:+file), size.toInt))
      }
    }

  fs.filter{ case ((t,_),_)=>  t =="d" }
    .toSeq
    .sortBy{ case ((_,p),_)=> -p.size }
    .foreach{ case ((t,dir),_) => fs.put((t,dir),fs.filter{case ((_,b),_) => b.init==dir}.values.sum)}

  val part1 = fs.filter(_._1._1 == "d").filter(_._2 < 100000).values.sum
  println(s"part1 ${part1}")

  val unused_space = 70000000 - fs(("d",Seq("/")))
  val sill_to_go = 30000000 - unused_space
  val part2 = fs.filter{
     case (("d",_),s) => s >= sill_to_go
     case _ => false
   }.reduce((a,b)=> if(a._2 < b._2) a else b)
  println(s"part2 $part2")
}
