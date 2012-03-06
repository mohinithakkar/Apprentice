package error

import parse._
import utils._

class Freedom {

  def computeFreedom(storyList:List[Story], clusterList:List[Cluster], links:List[Link]):Double = 
  {
    val clusters = links.flatMap { l => List(l.source, l.target) }.distinct.toArray
    val total = clusters.length
    val totalPairs = total * (total - 1) / 2.0
    
    var unconstrained = 0
    for(i <- 0 to total - 1)
    {
      for (j <- i + 1 to total - 1)
      {
        if (!Graph.ordered(links,clusters(i), clusters(j)))
            unconstrained += 1
      }
    }
    
    unconstrained / totalPairs
  }
}

object Freedom {
  
}