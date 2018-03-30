//https://github.com/vkostyukov/scalacaster/blob/master/src/graph/Graph.scala

  /**
   * Returns all nodes of this graph arranged by BFS algorithm.
   *
   * Time - O()
   * Space - O()
   */

  def nodesByBreadth: List[N] = graphsByBreadth.map(_.value)

  /**
   * Returns all graphs that are connected to this graph arranged by BFS algorithm.
   *
   * Time - O()
   * Space - O()
   */

  def graphsByBreadth: List[Graph[E, N]] = {   
    def loop(q: Queue[Graph[E, N]], s: Set[Graph[E, N]]): Set[Graph[E, N]] =
      if (!q.isEmpty && !s(q.head)) {
        val qq = q.head.succs.foldLeft(q.tail)((acc, gg) => acc :+ gg)
        loop(q.head.preds.foldLeft(qq)((acc, gg) => acc :+ gg), s + q.head)
      } else s

    loop(Queue(this), Set()).toList
  }
  
  
  