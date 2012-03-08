package graph

import data._

/** contains graph related algorithms
 * 
 */
object GraphAlgo {
  
  /** returns if cluster1 and 2 are ordered on the given graph, which is described by the links
   * 
   */
  def ordered(links: List[Link], cluster1: Cluster, cluster2: Cluster):Boolean =
    findShortestDistance(links, cluster1, cluster2) != -1 || findShortestDistance(links, cluster2, cluster1) != -1
    
    
	/**
   * find the shortest part distance between the two nodes based on the graph
   * if source cannot be reached from target, return -1
   *
   */
  def findShortestDistance(links: List[Link], source: Cluster, target: Cluster): Int =
    {
      var debug = false
      //if (source.name == "choose restaurant" && target.name == "eat food") debug = true
      // a breadth-first search
      var longest = -1
      val queue = scala.collection.mutable.Queue[(Cluster, Int)]()
      var remaining = links
      queue += ((source, 0))

      while (queue != Nil) {
        val elem = queue.dequeue()

        val head = elem._1
        val dist = elem._2
        if (debug) println("dequeue: " + head.name + " " + dist)
        if (head == target) {
          return dist
        } else {
          links.filter(link => link.source == head).foreach {
            link =>
              queue.enqueue((link.target, dist + 1))
              if (debug) println("enqueue: " + link.target.name + " " + (dist + 1))
          }
        }
      }
      //println("distance from " + source.name + " to " + target.name + " = " + longest)
      -1
    }
  
    /**
   * find the diameter between the two nodes based on the graph
   *
   */
  def findDiameter(links: List[Link], source: Cluster, target: Cluster): Int =
    {
      var debug = false
      //if (source.name == "choose restaurant" && target.name == "eat food") debug = true
      // a breadth-first search
      var longest = -1
      val queue = scala.collection.mutable.Queue[(Cluster, Int)]()
      var remaining = links
      queue += ((source, 0))

      while (queue != Nil) {
        val elem = queue.dequeue()

        val head = elem._1
        val dist = elem._2
        if (debug) println("dequeue: " + head.name + " " + dist)
        if (head == target) {
          if (dist > longest) longest = dist
        } else {
          links.filter(link => link.source == head).foreach {
            link =>
              queue.enqueue((link.target, dist + 1))
              if (debug) println("enqueue: " + link.target.name + " " + (dist + 1))
          }
        }
      }
      //println("distance from " + source.name + " to " + target.name + " = " + longest)
      longest
    }

}