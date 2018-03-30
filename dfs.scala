//https://github.com/vkostyukov/scalacaster/blob/master/src/graph/Graph.scala

  def nodesByDepth: List[N] = graphsByDepth.map(_.value)
  
  def graphsByDepth: List[Graph[E, N]] = {
    def loop(g: Graph[E, N], s: Set[Graph[E, N]]): Set[Graph[E, N]] = 
      if (!s(g)) {
        val ss = g.succs.foldLeft(s + g)((acc, gg) => loop(gg, acc))
        g.preds.foldLeft(ss)((acc, gg) => loop(gg, acc))
      } else s

    loop(this, Set()).toList
  }
