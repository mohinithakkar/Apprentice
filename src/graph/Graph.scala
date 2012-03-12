package graph

import data._
import java.io._
import scala.collection.immutable.HashMap

class Graph(val nodes: List[Cluster], val links: List[Link]) extends XStreamable{
  
  override def alias() = "plot-graph"
  /** returns if cluster1 and 2 are ordered on the given graph, which is described by the links
   *
   */
  def ordered(cluster1: Cluster, cluster2: Cluster): Boolean =
    shortestDistance(cluster1, cluster2) != -1 || shortestDistance(cluster2, cluster1) != -1

  /** find the shortest part distance between the two nodes based on the graph
   *  if source cannot be reached from target, return -1
   *
   */
  def shortestDistance(source: Cluster, target: Cluster): Int =
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
        }
        else {
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

  /** find the diameter between the two nodes based on the graph
   *
   */
  def diameter(source: Cluster, target: Cluster): Int =
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
        }
        else {
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

  /** eliminates redundant nodes, which do not appear in any links
   *
   */
  def reduce(): Graph =
    {
      val newNodes = usedClusters
      new Graph(newNodes, links)
    }

  def usedClusters() = links.flatMap { link => List(link.source, link.target) }.distinct

  /** simplifies the graph to a simple graph. When A->B and B->C are both present, A->C is omitted.
   *  The reduction only applies to links of the same kind
   */
  def simplify(): Graph = {

    // simplifying the graph
    val pairs = (0 until nodes.length) zip nodes
    val num2cluster = HashMap(pairs: _*) // mapping from number to cluster object
    val cluster2num = num2cluster.map(_.swap) // mapping from a cluster object to its number

    val temporals = links filter { _.isTemporal } map {
      link =>
        val id1 = cluster2num(link.source)
        val id2 = cluster2num(link.target)
        (id1, id2)
    }

    val causals = links filter { _.isCausal } map {
      link =>
        val id1 = cluster2num(link.source)
        val id2 = cluster2num(link.target)
        (id1, id2)
    }

    // this is a helper function that delete redundant links
    def reduceLinks = (numbers: List[(Int, Int)]) => {

      if (numbers == Nil) Nil
      else {
        val order = new Ordering(numbers.toSet[(Int, Int)])
        order.necessary()
      }

    }

    val newLinks =
      reduceLinks(temporals).map { l => new Link(num2cluster(l._1), num2cluster(l._2), "T") }.toList ++
        reduceLinks(causals).map { l => new Link(num2cluster(l._1), num2cluster(l._2), "C") }

    new Graph(nodes, newLinks)
  }

  // reduce + simplify
  def compact(): Graph = reduce.simplify

  /** draws the diagram to disk
   *
   */
  def draw(filename: String) {

    val file = new File(filename)
    val writer = new PrintWriter(new BufferedOutputStream(new FileOutputStream(file)))
    writer.println("digraph G {")
    writer.println(links.mkString("\n"))
    writer.println("}")
    writer.close()

    val outputName = filename.substring(0, filename.lastIndexOf("."))
    Runtime.getRuntime().exec("dot -Tpng -o" + outputName + ".png " + filename)
    file.deleteOnExit()
  }
  
   def takeSteps(source: Cluster, steps: Int): List[Cluster] =
    {
      var ends = List[Cluster]()

      val distance = scala.collection.mutable.Queue[(Cluster, Int)]()
      var remaining = links
      distance += ((source, 0))

      while (distance != Nil) {
        val elem = distance.dequeue()
        val head = elem._1
        val dist = elem._2
        if (dist == steps) ends = head :: ends // find one end. add it to the list
        else {
          links.filter(link => link.source == head).foreach {
            link => distance.enqueue((link.target, dist + 1))
          }
        }
      }

      ends.distinct
    }

}

/** contains graph related algorithms
 *
 */
object GraphAlgo {

  /** returns if cluster1 and 2 are ordered on the given graph, which is described by the links
   *
   */
  def ordered(links: List[Link], cluster1: Cluster, cluster2: Cluster): Boolean =
    findShortestDistance(links, cluster1, cluster2) != -1 || findShortestDistance(links, cluster2, cluster1) != -1

  /** find the shortest part distance between the two nodes based on the graph
   *  if source cannot be reached from target, return -1
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
        }
        else {
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

  /** find the diameter between the two nodes based on the graph
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
        }
        else {
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