//http://r-h.hatenablog.com/entry/2014/01/14/224338

import scala.collection.immutable.{Stack, Queue}
import scala.annotation.tailrec

//Edgeは枝・辺

case class Graph(es: Map[Vertex, Seq[Edge]])
case calss Vertex(i: Int)
case class Edge(v: Vertex)


// 幅優先探索
//       |3|
//   |1|     |2|
// |5| |4| |6|
//
// 3>1>2>5>4>6の順に探索
//
// queue[3]
// 3 : queue[1,2]
// 1 : queue[2,5,4]
// 2 : queue[5,4,6]
// 5 : queue[4,6]
// 4 : queue[6]
// 6

def sampleGraph: Graph = Graph(
  Map()
  Map(
    Vertex(3) -> Seq(Edge(Vertex(1)), Edge(Vertex(2))),
    Vertex(1) -> Seq(Edge(Vertex(5)), Edge(Vertex(4))),
    Vertex(2) -> Seq(Edge(Vertex(6)))
  )
)

def bfs(g: Graph, v: Vertex, goal: Vertex): Boolean = {
  bfs(g, goal, Set(), Queue(v))
}
@tailrec
def bfs(g: Graph, goal: Vertex, visited: Set[Vertex], queue: Queue[Vertex]): Boolean = {

//http://scala-search.org/?q=%24&m=org.scala-lang%3Ascala-library%3A2.11.7&m=org.scalaz%3Ascalaz-core_2.11%3A7.1.1
//a dollar sign ($) followed by a number will be interpreted as a reference to a group in the matched pattern

//メソッド dequeue は、キューにデータがあるかチェックしてから、front の位置にあるデータを取り出して変数 x にセットします。
//http://www.geocities.jp/m_hiroi/java/scala14.html
//http://wa3.i-3-i.info/word14722.html

  val (v, q) = queue.dequeue; println(s"v:${v} q:${q}")
  if (v == goal) true
  
//Scalaで単純に要素の存在チェックをするときはcontainsを使います。
//http://pskl.hatenablog.com/entry/2015/09/06/Scala%E3%81%AEcontains%E3%81%A8exists%E3%81%AE%E9%81%95%E3%81%84%E3%82%92%E8%A6%9A%E3%81%88%E3%81%9F%21%21
//scala> List(1,2,3,4,5).contains(1)
// result
//true 
 
  
  else if (visited.contains(v)) false
  else {
    g.es.get(v) match {
    
// /: is foldLeft
//https://dev.classmethod.jp/server-side/scala-foldright-foldleft/

//Optionとは値があるかどうかわからない状態を表すための型です。
//Optionのサブクラスには値があるときのSome(value)と、値がないことを表すNoneがあります。
//Optionは値がnull以外の時あるときはSome(value)、値がない場合nullの場合はNoneを返します。
//http://engineer-hiko.hatenablog.com/entry/2014/09/12/215529

      case Some(e) => bfs(g, goal, visited + v, e./:(q){(qq, ee) => qq.enqueue(ee.v)})
      case _ if q.nonEmpty => bfs(g, goal, visited + v, q)
      case _ => false
    }
  }
}