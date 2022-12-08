
val x = 2
val s = Seq(2,5,5,1,2).zipWithIndex
val (l,r) = s.splitAt(x)
// (coord, score)
val rScore = r.tail.foldLeft((0,0))( (acc,el) =>if (acc._1 < el._1) (el._1,acc._2+1) else acc)
val lScore = l.foldRight((0,0))( (acc,el) =>if (acc._1 < el._1) (el._1,acc._2+1) else acc)

println(lScore, rScore)


