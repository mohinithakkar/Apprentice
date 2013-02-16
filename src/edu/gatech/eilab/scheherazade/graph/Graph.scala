package edu.gatech.eilab.scheherazade

import data._
import java.io._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

package graph {
  class Graph(val nodes: List[Cluster], val links: List[Link], val mutualExcls:List[MutualExcl]) extends XStreamable {

    def this(nodes: List[Cluster], links: List[Link]) = this(nodes, links, List[MutualExcl]())
    // this is used in XStreamable
    override def alias() = "plot-graph"

    def causalLinks() = links.filter(_.isCausal)
    def temporalLinks() = links.filter(_.isTemporal)
    def usedClusters() = links.flatMap { link => List(link.source, link.target) }.distinct

    def makeEfficient(): EfficientGraph =
      {
        new EfficientGraph(nodes, links)
      }

    /**
     * returns if cluster1 and 2 are ordered on the given graph, which is described by the links
     *
     */
    def ordered(cluster1: Cluster, cluster2: Cluster): Boolean =
      shortestDistance(cluster1, cluster2) != -1 || shortestDistance(cluster2, cluster1) != -1

    /**
     * find the shortest part distance between the two nodes based on the graph
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

    /**
     * eliminates redundant nodes, which do not appear in any links
     *
     */
    def reduce(): Graph =
      {
        val newNodes = usedClusters
        new Graph(newNodes, links, mutualExcls)
      }

    /**
     * simplifies the graph to a simple graph. When A->B and B->C are both present, A->C is omitted.
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

      new Graph(nodes, newLinks, mutualExcls)
    }

    // reduce + simplify
    def compact(): Graph = reduce.simplify

    // replace temporal with causal links so that there is at most one link between two clusters
    def singleLink(): Graph = {
      val causal = causalLinks()
      var temporal = temporalLinks()

      temporal = temporal.filterNot { tl => causal.exists { c => c.target == tl.target && c.source == tl.source } }

      new Graph(nodes, temporal ::: causal, mutualExcls)
    }

    /**
     * returns the direct predecessors of a graph node
     *
     */
    def predecessorsOf(c: Cluster): List[Cluster] =
      {
        if (!nodes.contains(c))
          throw new GraphException("The graph does not contain the node specified: " + c.name)
        else {
          links.filter(_.target == c).map(_.source)
        }
      }

    /**
     * remove nodes from the graph and any links involving these nodes
     */
    def removeNodes(excluded: List[Cluster]): Graph =
      {
        val newLinks = links.filterNot(l => excluded.contains(l.source) || excluded.contains(l.target))
        val newExcls = mutualExcls.filterNot(m => m.c1 == excluded || m.c2 == excluded)
        new Graph(nodes filterNot (excluded contains), newLinks, newExcls)
      }

    /**
     * add links from the predecessors of the events to the successors of the events
     *
     */
    def addSkipLinks(events: List[Cluster]): Graph =
      {
        var newLinks = ListBuffer[Link]() ++ links
        for (e <- events) {
          val predecessors = newLinks.filter(l => l.target == e).map(_.source)
          val successors = newLinks.filter(l => l.source == e).map(_.target)
          for (p <- predecessorsOf(e); s <- successors)
           {
            newLinks += new Link(p, s)            
           }
        }
        
        new Graph(nodes, newLinks.toList, this.mutualExcls)
      }

    /**
     * draws the diagram to disk
     *
     */
    def draw(fn: String) {

      val filename = fn + ".txt"
      val file = new File(filename)
      val writer = new PrintWriter(new BufferedOutputStream(new FileOutputStream(file)))
      writer.println("digraph G {")
      //writer.println(causalLinks.map { l => "\"" + l.source.name + "\" -- \"" + l.target.name + "\" [style = \"dashed\"]" }.mkString("\n"))
      writer.println(mutualExcls.map {m => "\"" + m.c1.name + "\" -> \"" + m.c2.name + "\" [style=dashed, dir=none]" }.mkString(";\n"))
      writer.println(temporalLinks.map { l => "\"" + l.source.name + "\" -> \"" + l.target.name + "\"" }.mkString("\n"))
      //writer.println(mutualExcls.map { m => "\"" + m.c1.name + """" -- [style = "dashed"]" """ + m.c2.name + "\""}.mkString(";\n"))      
      writer.println("}")
      writer.close()

      println("writing graph to " + fn + ".png")
      Runtime.getRuntime().exec("dot -Tpng -o" + fn + ".png " + filename)
      //file.deleteOnExit()
    }

  }

  /**
   * Contains a number of sample graphs. Graph algorithms can be easily tested on them.
   *
   */
  object SampleGraph {

    /**
     *      1
     *      /  \
     *     2    3
     *         / \
     *        4   5
     *         \  /
     *           6
     *          /
     *         7
     */
    val sample1: Graph = {
      val c1 = new Cluster("C1", Nil)
      val c2 = new Cluster("C2", Nil)
      val c3 = new Cluster("C3", Nil)
      val c4 = new Cluster("C4", Nil)
      val c5 = new Cluster("C5", Nil)
      val c6 = new Cluster("C6", Nil)
      val c7 = new Cluster("C7", Nil)

      val links = List(
        new Link(c1, c2),
        new Link(c1, c3),
        new Link(c3, c4),
        new Link(c3, c5),
        new Link(c4, c6),
        new Link(c5, c6),
        new Link(c6, c7))

      new Graph(List(c1, c2, c3, c4, c5, c6, c7), links)
    }

    /**
     *      2        5
     * 1 -> { 3 } -> { 6 } -> 8
     *        4        7
     *
     * Each node in {2, 3, 4} is connected to 2 nodes in {5, 6, 7}
     */
    val sample2: Graph = {
      val c1 = new Cluster("C1", Nil)
      val c2 = new Cluster("C2", Nil)
      val c3 = new Cluster("C3", Nil)
      val c4 = new Cluster("C4", Nil)
      val c5 = new Cluster("C5", Nil)
      val c6 = new Cluster("C6", Nil)
      val c7 = new Cluster("C7", Nil)
      val c8 = new Cluster("C8", Nil)

      val links = List(
        new Link(c1, c2),
        new Link(c1, c3),
        new Link(c1, c4),
        new Link(c2, c5),
        new Link(c2, c6),
        new Link(c3, c6),
        new Link(c3, c7),
        new Link(c4, c7),
        new Link(c4, c5),
        new Link(c5, c8),
        new Link(c6, c8),
        new Link(c7, c8))

      new Graph(List(c1, c2, c3, c4, c5, c6, c7, c8), links)
    }
  }
}