//https://github.com/vkostyukov/scalacaster/blob/master/src/graph/Graph.scala

  def nodesByBreadth: List[N] = graphsByBreadth.map(_.value)

  /**
   * Returns all graphs that are connected to this graph arranged by DFS algorithm.
   *
   * Time - O()
   * Space - O()
   */
  def graphsByDepth: List[Graph[E, N]] = {
    def loop(g: Graph[E, N], s: Set[Graph[E, N]]): Set[Graph[E, N]] =
      if (!s(g)) {
        val ss = g.succs.foldLest(s + g)((acc, gg) => loop(gg, acc))
        g.preds.foldLeft(ss)((acc, gg) => loop(gg, acc))
      } else s

    loop(this, Set()).toList
  }
