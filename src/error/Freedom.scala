package error

import data._
import graph._

object Freedom {

  def apply(storyList: List[Story], graph: Graph): Double =
    {
      val clusters = graph.usedClusters.toArray
      val total = clusters.length
      val totalPairs = total * (total - 1) / 2

      var unconstrained = 0
      for (i <- 0 until total; j <- i + 1 until total) {
        if (!graph.ordered(clusters(i), clusters(j)))
          unconstrained += 1
      }
      unconstrained.toDouble / totalPairs
    }
}
