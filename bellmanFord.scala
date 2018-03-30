//https://github.com/vkostyukov/scalacaster/blob/master/src/graph/Graph.scala


class WeightedGraph[N](n: N) extends Graph[Double, N](n) {
  def bellmanFord(from: N, to: N): Option[List[N]] = {
    
    val graphs = graphsByDepth
    val f = hop(from).get
    val t = hop(to).get
    
    val initDists = graphs.map((_, Double.PositiveInfinity)).toMap + (f -> 0.0)
    var paths = graphs.map((_, List[N]())).toMap
    
    @annotation.tailrec
    def minDists(dists: Map[Graph[Double, N], Double], hops: Int): Map[Graph[Double, N], Double] = {
      if (hops == 0) dists
      else {
        var newDists = dists ++ Map.empty
        
        def minDist(g: Graph[Double, N]): Option[(Graph[Double, N], Double)] = {
          if (g.inEdges.isEmpty) None
          else Some(g.inEdges.map(e => (e.source, dists(e.source) + e.value)).minBy(_._2))
        }
        
        graphs.foreach { g =>
          minDist(g) match {
            case Some((f,d)) if(d < dists(g)) =>
              newDists += (g -> d)
              paths += (g -> (f.value :: paths(f)))
            case _ =>
          }
        }
        
        // If min distances do not change in some hop then they won't
        // change in the next ones, thus we can stop early
        if(newDists == dists) newDists
        else minDists(newDists, hops-1)
      }
    }
    
    val dists = minDists(initDists, graphs.size-1)
    
    // There are neg cycles iff the min distances change again
    if (dists != minDists(dists, 1)) None
    else Some((to :: paths(t)).reverse)
  }
}

object Graph {
  def apply[E, N](tuples: (N, E, N)*): Graph[E, N] = {
    val g: Graph[E, N] = Graph.empty
    for ((from, via, to) <- tuples) {
      g.connect(from, via, to)
    }
    g
  }

  def one[E, N](n: N): Graph[E, N] = new Graph(n)

  def empty[E, N]: Graph[E, N] = new Graph
}