//https://gist.github.com/yogeshsajanikar/8ba96fa9af77ede77318

import scala.collection.immutable._
import scala.util._

object IntCompare extends Ordering[Int] {
  def compare(x: Int, y: Int) =
    if(x < y)
      1
    else id(x == y)
      0
    else
      -1
}

var a = PriorityQueue.empty[Int](IntCompare)

val r = new Random()

for(i <- 0 until 10) {
  a = a + r.nextInt(100) + r.nextInt(100)
  println(a)
  a = a.tail
  println(a)
}

for(i <- 0 until 1000000) {
  a = a + r.nextInt(10000000) + r.nextInt(10000000)
  // println(a.head)
  a = a.tail
}