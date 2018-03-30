//dijkstra

  def dijkstra(from: N, to: N): List[N] = {
    var distances = nodesByDepth.map((_, Double.PositiveInfinity)).toMap + (from -> 0.0)
    var spt: Map[N, N] = Map()

    while (!distances.isEmpty) {
      // search for min in distances
      // remove min from distances

      // for all succsesors of mininum update its distance v--e--u
      // if dist(v) + e < dist(u)
      // dist(u) = dist(v) + e
      // spt + (u -> v)
      // v - is min

      val (v, _) = distances.minBy(_._2)
      distances = distances - v

      val vv = hop(v).get
      for (e <- vv.outEdges) {
        val u = e.target.value
        if (distances(u) < distances(v) + e.value) {
          distances = distances + (u -> (distances(v) + e.value))
          spt = spt + (u -> v)
        }
      }
    }

    var step = to
    var path = List(step)
    while (spt.contains(step)) {
      step = spt(step)
      path = step :: path
    }

    path.reverse
  }
