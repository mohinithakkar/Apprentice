package edu.gatech.eilab.scheherazade

import data._
import java.io._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

package graph {
  class Graph(val nodes: List[Cluster], val links: List[Link], val mutualExcls: List[MutualExcl]) extends XStreamable {

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
            newLinks += new Link(p, s)
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
      writer.println(mutualExcls.map { m => "\"" + m.c1.name + "\" -> \"" + m.c2.name + "\" [style=dashed, dir=none]" }.mkString(";\n"))
      writer.println(temporalLinks.map { l => "\"" + l.source.name + "\" -> \"" + l.target.name + "\"" }.mkString("\n"))
      //writer.println(mutualExcls.map { m => "\"" + m.c1.name + """" -- [style = "dashed"]" """ + m.c2.name + "\""}.mkString(";\n"))      
      writer.println("}")
      writer.close()

      println("writing graph to " + fn + ".png")
      Runtime.getRuntime().exec("dot -Tpng -o" + fn + ".png " + filename)
      file.deleteOnExit()
    }

    def numberNodes() = {
      var node2Num = Map[Cluster, Int]()
      var num2Node = Map[Int, Cluster]()

      var i = 0
      nodes.foreach {
        node =>
          node2Num += (node -> i)
          num2Node += (i -> node)
          i += 1
      }

      (num2Node, node2Num)
    }

    def containsLoop(): Boolean = {
      val length = nodes.size
      var visited = Array.fill[Boolean](length)(false)
      var stack = Stack[Int]()
      val (num2Node, node2Num) = numberNodes()

      stack.push(0)

      while (!stack.isEmpty) {
        val start = stack.top
        if (visited(start)) {
          // visiting the node for the second time
          stack.pop()
        } else {
          // visiting the node for the first time
          visited(start) = true

          val outgoing = links.filter(l => l.source == num2Node(start))
          outgoing.foreach {
            link =>
              //println("link = " + link.source.name + " " + link.target.name)
              val end = node2Num(link.target)
              // found a loop if we can reach one of the parent. It is visited so it is not a sibling
              if (stack.contains(end) && visited(end)) {
                //println("reached " + end + " from " + start)
                return true
              } else
                stack.push(end)
          }
        }
      }
      false
    }

    /**
     * find simple loops in the graph.
     * We assume that the loops in the graph does not share any edges
     */
    def simpleLoops(): List[List[Cluster]] = {
      val length = nodes.size
      var visited = Array.fill[Boolean](length)(false)
      var stack = Stack[Int]()
      val (num2Node, node2Num) = numberNodes()
      var loopList = List[List[Int]]()

      var parentTrack = Array.fill[Int](length)(-1)

      def runOneComponent(init: Int)
      {
        stack.push(init)

        while (!stack.isEmpty) {
          val start = stack.pop
          // visiting the node for the first time
          visited(start) = true
          //println("visit " + start)
          val outgoing = links.filter(l => l.source == num2Node(start))
          outgoing.foreach {
            link =>
              //println("link = " + link.source.name + " " + link.target.name)
              val end = node2Num(link.target)

              // find all parents
              var parents = List(start)
              var ptr = start
              while (parentTrack(ptr) != -1 && ptr != end) {
                ptr = parentTrack(ptr)
                parents = ptr :: parents
              }

              // found a loop if we can reach one of the parent. It is visited so it is not a sibling
              if (parents.contains(end)) {
                val loop = parents
                // add to loop list
                loopList = loop :: loopList
              } else if (!visited(end)) {
                stack.push(end)
                parentTrack(end) = start                
              }
          }
        }
      }
      
      for(i <- 0 until length)
      {
        if (!visited(i))
          runOneComponent(i)
      }

      loopList.map(_.map(num2Node))
    }

    def allLoops(): List[List[Cluster]] = {
      val length = nodes.size
      var visited = Array.fill[Int](length)(0)
      var stack = Stack[Int]()
      val (num2Node, node2Num) = numberNodes()
      var loopList = List[List[Int]]()
      var curloop = 1 // current loop
      var parentTrack = Array.fill[Int](length)(-1)

      var parentStack = Stack[Int]()

      val init = 0 //math.floor(math.random * length).toInt
      println("init = " + init)
      stack.push(init)

      while (!stack.isEmpty) {
        val start = stack.pop
        // visiting the node for the first time
        visited(start) = curloop
        //println("visit " + start)
        val outgoing = links.filter(l => l.source == num2Node(start))
        outgoing.foreach {
          link =>
            //println("link = " + link.source.name + " " + link.target.name)
            val end = node2Num(link.target)

            // find all parents
            var parents = List(start)
            var ptr = start
            while (parentTrack(ptr) != -1 && ptr != end) {
              ptr = parentTrack(ptr)
              parents = ptr :: parents
            }

            // found a loop if we can reach one of the parent. It is visited so it is not a sibling
            if (parents.contains(end)) {
              //println("reached " + end + " from " + start)
              //var parents = stack.toList.filter(visited(_) == curloop).reverse
              // loop = parents list from end to start
              //val loop = parents.dropWhile(_ != end)
              val loop = parents
              //println("found loop: " + loop)
              // add to loop list
              loopList = loop :: loopList

              // mark all elements in the loop with a higher curloop
              //curloop += 1
              //loop.foreach(l => visited(l) = curloop)

            } else if (visited(end) < curloop) {
              stack.push(end)
              parentTrack(end) = start
              //println("push " + end)
              //parentStack.push(end)
            }
        }

      }

      loopList.map(_.map(num2Node))
    }

    def tarjan() {
      new Tarjan(this).run()
    }

  }

  class Tarjan(val graph: Graph) {

    val length = graph.nodes.size
    val (num2Node, node2Num) = graph.numberNodes()

    var highInd = 1;
    val stack: Stack[Int] = Stack[Int]()
    val lowlink = Array.fill[Int](length)(0)
    val index = Array.fill[Int](length)(0)

    var loopInd = 0
    var loopMember = scala.collection.mutable.HashMap[Int, List[Int]]()
    var loopList = List[List[Int]]()

    def run() {

      for (i <- 0 until length) {
        loopMember += (i -> List[Int]())
      }

      for (i <- 0 until length) {
        if (index(i) == 0) {
          strongconnect(i)

        }
      }

      println(loopMember.map {
        case (id, list) =>
          num2Node(id).name + " -> " + list
      })

      println("loop List = ")
      println(loopList.map(_.map(num2Node(_).name)))
    }

    private def strongconnect(v: Int) {
      index(v) = highInd
      lowlink(v) = highInd
      highInd = highInd + 1
      stack.push(v)

      println("visiting " + v)
      println("stack: " + stack)

      val node = num2Node(v)
      val outgoing = graph.links.filter(_.source == node)

      outgoing foreach {
        link =>
          val end = node2Num(link.target)

          println("links from " + v + " to " + end)
          if (index(end) == 0) {
            // end has not been visited
            strongconnect(end)
            if (lowlink(end) < lowlink(v)) {
              lowlink(v) = lowlink(end)
            }

          } else if (stack.contains(end)) {
            // this is the termination of recursion.
            // we have found a loop here            
            lowlink(v) = math.min(lowlink(v), lowlink(end))

            loopInd += 1

            // everything on the stack are included in the loop
            stack.foreach {
              k =>
                var list = loopMember(k)
                list = loopInd :: list
                loopMember(k) = list
            }

            loopList = stack.toList.reverse :: loopList
            println("all loops: " + loopList)
            //println(loopMember)
            println("from " + v + " reached " + end + ". loop!")
            //stack.pop()
          } else if (!loopMember(end).isEmpty) {
            val firstPart = stack.toList.reverse

            val secondLoops = loopList.filter(_.contains(end)).map(_.dropWhile(_ != end)).distinct
            println("s = " + secondLoops)
            secondLoops foreach {
              list =>
                val fullLoop = firstPart ::: list
                loopList = fullLoop :: loopList
                println("all loops: " + loopList)
            }

            println("from " + v + " reached " + end + ". loop!")
          }
      }

      stack.pop

      //      if (lowlink(v) == index(v)) {
      //        val component = ListBuffer[Int]()
      //        var w = -1
      //        do {
      //          w = stack.pop()
      //          component += w
      //        } while (w != v)
      //        println("component = " + component.map(x => num2Node(x).name).mkString("(", ", ", ")"))
      //      }
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

    /**
     * a graph with a simple loop: 1 -> 2 -> 3 -> 1, but also contains a non-loop part: 2->4->3
     */
    val loopGraph1: Graph = {
      val c1 = new Cluster("C1", Nil)
      val c2 = new Cluster("C2", Nil)
      val c3 = new Cluster("C3", Nil)
      val c4 = new Cluster("C4", Nil)

      val links = List(
        new Link(c1, c2),
        new Link(c2, c3),
        new Link(c3, c1),
        new Link(c2, c4),
        new Link(c4, c3))

      new Graph(List(c1, c2, c3, c4), links)
    }

    /**
     * very similar to loopGraph1, but has no loops. 1->2->3, 2->4->3
     */
    val noloopGraph1: Graph = {
      val c1 = new Cluster("C1", Nil)
      val c2 = new Cluster("C2", Nil)
      val c3 = new Cluster("C3", Nil)
      val c4 = new Cluster("C4", Nil)

      val links = List(
        new Link(c1, c2),
        new Link(c2, c3),
        new Link(c2, c4),
        new Link(c4, c3))

      new Graph(List(c1, c2, c3, c4), links)
    }

    /**
     *
     * a graph with two loops: 0->1->2->3->0, 1->4->5->3, 4->7->3
     * two loops shared edges: 0->1, 3->0
     * addition branch: 5 -> 6
     */
    val loopGraph2: Graph = {
      val c1 = new Cluster("C0", Nil)
      val c2 = new Cluster("C1", Nil)
      val c3 = new Cluster("C2", Nil)
      val c4 = new Cluster("C3", Nil)
      val c5 = new Cluster("C4", Nil)
      val c6 = new Cluster("C5", Nil)
      val c7 = new Cluster("C6", Nil)
      val c8 = new Cluster("C7", Nil)

      val links = List(
        new Link(c1, c2),
        new Link(c2, c3),
        new Link(c3, c4),
        new Link(c4, c1),
        new Link(c2, c5),
        new Link(c5, c6),
        new Link(c6, c4),
        new Link(c5, c8),
        new Link(c8, c4),
        new Link(c6, c7))

      new Graph(List(c1, c2, c3, c4, c5, c6, c7, c8), links)
    }

    /**
     *
     * a graph with one loop: 1->2->3->1
     * addition edges: 0->1, 0->3
     */
    val loopGraph3: Graph = {
      val c1 = new Cluster("C0", Nil)
      val c2 = new Cluster("C1", Nil)
      val c3 = new Cluster("C2", Nil)
      val c4 = new Cluster("C3", Nil)

      val links = List(
        new Link(c1, c2),
        new Link(c2, c3),
        new Link(c3, c4),
        new Link(c4, c2),
        new Link(c1, c4))

      new Graph(List(c1, c2, c3, c4), links)
    }

    def main(args: Array[String]) {
      println(sample1.containsLoop())
      println(sample2.containsLoop())
      println(loopGraph1.containsLoop())
      println(noloopGraph1.containsLoop())
      println(loopGraph2.containsLoop())
      /*
      for (i <- 0 to 10) {
        val all = loopGraph2.allLoops().map(_.map(_.name))
        //println(all)
        var good = all.exists(l => l.filterNot(List("C1", "C2", "C3", "C0") contains).size == 0)
        good = good && all.exists(l => l.filterNot(List("C1", "C4", "C5", "C0", "C3") contains).size == 0)
        good = good && all.exists(l => l.filterNot(List("C1", "C4", "C7", "C0", "C3") contains).size == 0)
        good = good && all.size == 3
        println("good ? " + good)
      }
      */

      println(loopGraph3.allLoops())
    }
  }
}