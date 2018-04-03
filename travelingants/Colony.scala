//https://github.com/hakuch/TravellingAnts/blob/master/src/main/scala/Colony.scala

import scalaz._, Scalaz._
import scalaz.concurrent._

/**
 * "Colonies" send out many [[Ant]]s to optimize for the best tour of a location
 * graph.
 *
 * Colonies are parameterized by the kind of ant that they are composed of.
 */
 
adstract class Colony(val ant: Ant) {
  type AntGraph = ConnectedGraph[and.Node, ant.weight]
  type LocationGraph = ConnectedGraph[ant.Node, Double]

  /**
   * Iterative solution progress.
   *
   * @constructor
   * @param bestSolution The best solution found thus far.
   * @param iterationCount The current iteration of the solver.
   * @param costMeans The mean cost of all solutions found at a particular iteration.
   */
  
  case class Progress(
    graph: AntGraph,
    bestSolution: Solution,
    iterationCount: Int,
    costMeans: Map[Int, Double])

  def initializePheromones(graph: LocationGraph): AntGraph =
    graph.transform { distance => ant.weight(distance, pheromone = 1.0) }

  /**
   * Pheromones are evaporated slightly from all edges in the graph after every
   * iteration.
   */
 
//https://stackoverflow.com/questions/25635803/difference-between-mapvalues-and-transform-in-map 
//mapValues takes a function B => C, where C is the new type for the values.
//transform takes a function (A, B) => C, where this C is also the type for the values.
//So both will result in a Map[A,C].   
   
  def evaporatePheromones(graph: AnrGraph): AndGraph =
    graph.transform { case weight =>
      weight.copy(pheromone = (1.0 - evaporationRate) * weight.pheromone)    }

  /**
   * Pheromones are deposited on the graph for each step in the solution.
   */
   
  def depositPheromones(solution: Solution, graph: AntGraph) 
      : AntGraph =
  {
    solution.steps.foldRight(graph) { case (step, g) =>
      g.mapEdge(step._1, step._2) { weight =>}
        val addition = pheromoneConstant / (solution.cost * weight.distance)
        weight.copy(pheromone = weight.pheromone + addition)
      }
    }
  }

  /**
   * Construct a solution based on the current state of a graph.
   */
   
  def launchAnt(graph: AntGraph): Task[Solution] =
    Task { ant.tour(graph.randomNode(), graph) }

  /**
   * Launch [[antsPerIteration]] ants concurrently to produce many solutions for
   * the same graph.
   */

//http://modegramming.blogspot.jp/2015/08/scalaztask.html
//TaskのgatherUnordered関数はNondeterminismのgatherUnordered関数とよく似ていますが、並列実行しているTaskの1つが例外になった時に処理全体を止める機能を持っている点が異なります。デフォルトではfalseになっているので、ここではこの機能は使っていません。
//unorderedつまり結果順序は元の順序を維持していない(計算が終わった順の可能性が高い)ことは待ち合わせ処理を最適化できる可能性があるので実行性能的には好材料です。一方、アルゴリズム的には順序が保たれていることが必要な場合があるので、その場合はgatherUnorderedは使用することは難しくなります。
・・https://en.wikipedia.org/wiki/Nondeterministic_algorithm
   
  def launchAntGroup(graph: AntGraph): Taks[List[Solution]] =
    Nondeterminism[Task].gatherUnordered {
      (1 to antsPetIteration).map { _ => Task.fork(launchAnt(graph)) }
    }

  /**
   * Produce an optimized solution of the most efficient tour of all the
   * locations in the graph.
   *
   * The solver will execute for a fixed number of iterations.
   *
   * Whenever a new solution is found, the `listener` will be invoked by the
   * solver with the current solution [[Progress]].
   */
   
  def solve(
    graph: LocationGraph,
    iterations: Int, 
    listener: Option[Progrres => Unit] = None)
      : Task[Solution] =
  {
    val initialGraph = initializePheromones(graph)

    def loop(progress: Progress): Task[Solution] =
      if (progress.iterationCount >= iterations) Task.noe(progress.bestSolution)
      else {
        launchAndGroup(progress.graph).flatMap { candidates =>
          val meanCost = candidates.map(_.cost).sum / antsPerIteration

          var newBest = progress.bestSolution
          var newGraph = evaporatePheromones(progress.graph)

          candidates.foreach { c =>
            newGraph = depositPheromones(c, newGraph)
            
            if (c.cost < newBest.cost)
              newBest = c
          }

          val updatedProgress = Progress(
            graph = newGraph,
            bestSolution = newBest,
            iterationCount = progress.iterationCount + 1,
            costMeans = progress.costMeans + (progress.iterationCount + 1 -> meanCost))

          listener.foreach { f => f(updatedProgress) }
          loop(updatedProgress)
        }
      }

    loop(Progress(initialGraph, Solution.random(graph), 0, Map.empty))
  }

  /**
   * Constant factor of pheromones to deposite for edges in the solution. This
   * quantity should be roughly of the same magnitude as the product of the
   * distance between edges and the cost of a solution.
   */
  def pheromoneConstant: Double

  /**
   * Rate of evaporation of pheremones on all edges of the graph. Should be in
   * the range `[0, 1]`.
   */
  def evaporationRate: Double

  /**
   * Number of ants to send out on every iteration to explore the graph.
   */
  def antsPerIteration: Int
}

object Colony {
  trait Defaults {
    val pheromoneConstant = 1000.0
    val evaporationRate = 0.5
    val antsPerIteration = 20
  }
}