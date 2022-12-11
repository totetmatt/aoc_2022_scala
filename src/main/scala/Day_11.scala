import scala.collection.mutable
import scala.util.matching.Regex
case class Monkey(id:Int, items:Seq[Long], operation: Long => Long, test: (Long) => Int, inspect:Int) {
  override def toString: String = s"(${id}: ${items}, ${inspect})"
}
object Day_11 extends App {

  val ops = Map(
    "+" ->((a:Long,b:Long) => a+b),
    "-" ->((a:Long,b:Long) => a-b),
    "/" ->((a:Long,b:Long) => a/b),
    "*" ->((a:Long,b:Long) => a*b),
  )
  val monkeyId = """Monkey (\d+):""".r
  val testDivisible: Regex = """^Test: divisible by ([0-9]+)$""".r
  val ifTrue = """If true: throw to monkey (\d+)""".r
  val ifFalse = """If false: throw to monkey (\d+)""".r
  val operation = """Operation: new = (\S+) (.) (\S+)""".r
  val monkeys : mutable.ArrayBuffer[Monkey] =mutable.ArrayBuffer.from(scala.io.Source.fromFile("input/day_11/input")
    .getLines
    .grouped(7)
    .map(
      _.map(_.strip).filter(_ != "")
        .map{
          case monkeyId(id) => id.toInt
          case b if b.contains("Starting items:") => b.replace("Starting items: ","").split(",").map(x=>x.strip.toLong).toSeq
          case b if b.contains("Operation:") => b match {
            case operation(left,op,right) => if(right=="old") {
              (old:Long) =>  ops(op)(old,old)
            } else {
              (old:Long) =>  ops(op)(old,right.toInt)
            }

          }
          case testDivisible(a) => a.toInt
          case ifTrue(id) => id.toInt
          case ifFalse(id) => id.toInt
        }
    )
    .map(x => {
      val id = x(0).asInstanceOf[Int]
      val start = x(1).asInstanceOf[Seq[Long]]
      val op = x(2).asInstanceOf[Long=>Long]
      val test = x(3).asInstanceOf[Int]
      val ifTrue:Int = x(4).asInstanceOf[Int]
      val ifFalse:Int = x(5).asInstanceOf[Int]
      Monkey(id,start,op,test= (i:Long) =>  if (i%test == 0) ifTrue else ifFalse,0)
    }
    ))

  {
    val monkeypart1 = mutable.ArrayBuffer.from(monkeys)
    for (x <- 1 to 20) {
      for ((_, idx) <- monkeypart1.zipWithIndex) {
        val m = monkeypart1(idx)
        println("----", m.id, "----")
        for (currentItemWorry <- m.items) {
          val newWorry = (m.operation(currentItemWorry) / 3)

          val goto = m.test(newWorry)
          println(s"new worry : ${currentItemWorry} -> ${newWorry} -> ${goto}")
          val nextMonkey = monkeypart1(goto)
          monkeypart1(goto) = nextMonkey.copy(items = nextMonkey.items :+ newWorry)


          println("---")
        }
        monkeypart1(idx) = m.copy(items = Seq.empty, inspect = m.inspect + m.items.size)
      }
      println(s"after round ${x}")
      monkeypart1.foreach(println)
    }

    println(monkeypart1.map(_.inspect).sorted.reverse.slice(0, 2).product)
  }

  {



    val monkeypart2 = mutable.ArrayBuffer.from(monkeys)
    for (x <- 1 to 10000) {
      for ((_, idx) <- monkeypart2.zipWithIndex) {
        val m = monkeypart2(idx)

        for (currentItemWorry <- m.items) {
          val newWorry = m.operation(currentItemWorry)

          val goto = m.test(newWorry)

          val nextMonkey = monkeypart2(goto)
          monkeypart2(goto) = nextMonkey.copy(items = nextMonkey.items :+ newWorry%(Seq(19,13,5,7,17,2,3,11).product))


        }
        monkeypart2(idx) = m.copy(items = Seq.empty, inspect = m.inspect + m.items.size)
      }

      if(x%100 ==0) println(x)
    }
    monkeypart2.foreach(println)
    println(monkeypart2.map(x=>x.inspect.toLong).sorted.reverse.slice(0, 2).product)
  }
  println(Seq(19,13,5,7,17,2,3,11).product)
}
