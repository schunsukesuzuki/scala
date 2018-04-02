//http://r-h.hatenablog.com/entry/2014/01/14/224338
//http://algorithms.blog55.fc2.com/blog-entry-52.html
//O(|V|3) または O(|V|(|E| + |V|)log|V|) 

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    val s = 3
    val m = Vector.fill(s, s)(0)
    val r = floid(makeData(m))
    printMatrix(r)
  }
  
  type Matrix = Vector[Vector[Int]]
  val INF = 9999

  def floyd(m: Matrix): Matrix = {
    def updated(d: Matrix, i: Int, j: Int, value: Int): Matrix = d.updated(i, d(i).updated(j, value))

    val vs = (0 until m.size).toList    
   
    @tailrec
    def fk(l: List[Int], d: Matrix): Matrix = l match {
      case k :: t => fk(t, fi(vs, d, k))
      case _ => d
    }
    @tailrec
    def fi(l: List[Int], d: Matrix, k: Int): Matrix = l match {
      case i :: t => fi(t, fj(vs, d, k, i), k)
      case _ => d
    }
    @tailrec
    def fj(l: List[Int], d: Matrix, k: Int, i: Int): Matrix = l match {
      case j :: t => {
        val m = math.min(d(i)(j), d(i)(k) + d(k)(j))
        fj(t, updated(d, i, j, m), k, i)
      }
      case _ => d
    }
    fk(vs, m)
  }

  private[this] def makeData(m: Matrix): Matrix = {
    val l = (0 until m.size).toVector
    val r = l.map { i =>
      l.map { j => if (i == j) 0 else INF }
    }
    List(_,_,_).foldLeft(r) { case (r, (i, j, weight)) => r.updated(i, r(i).updated(j, weight)) }
  }

  private[this] def printMatrix(d: Matrix): Unit = d.map {
    case i => i.map(j => print((if (j == INF) "F" else j) + " ")); println("")
  }
}