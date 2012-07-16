package graph

import data._
import scala.collection.mutable.ListBuffer

class EfficientGraph(override val nodes: List[Cluster], override val links: List[Link]) extends Graph(nodes: List[Cluster], links: List[Link]) {

  val node2Num: Map[Cluster, Int] = {
    val sorted = nodes.sortWith((x, y) => x.name > y.name) // introduce an arbitrary ordering between the clusters
    val num = 0 until nodes.length
    (sorted zip num).toMap
  }

  val num2Node: Map[Int, Cluster] = {
    node2Num.map { case (x, y) => (y, x) }
  }

  // the adjacency matrix. m(1, 2) = 1 if there is one arc from node 1 to node 2
  val adjacentMtx: Array[Array[Int]] = {
    val result = Array.ofDim[Int](nodes.length, nodes.length)
    links.foreach {
      l => result(node2Num(l.source))(node2Num(l.target)) += 1
    }

    result
  }

  private def getAdjacency(gNodes: List[Cluster], gLinks: List[Link]): Array[Array[Int]] = {

    val n = gNodes.length
    var result = Array.ofDim[Int](n, n)
    val array = gNodes.toArray
    for (link <- gLinks) {
      val id1 = array.indexOf(link.source)
      val id2 = array.indexOf(link.target)
      result(id1)(id2) += 1
    }

    links.foreach {
      l => result(node2Num(l.source))(node2Num(l.target)) += 1
    }

    result
  }

  // adjacent Stream
  val adjList: List[Array[Array[Int]]] = {
    import scala.collection.mutable.ListBuffer
    var buffer = ListBuffer[Array[Array[Int]]]()
    var prev = adjacentMtx
    var end = false
    while (!end) {
      buffer += prev
      prev = utils.Matrix.intMultiply(prev, adjacentMtx)
      // computation ends if the entire matrix contains only zeros
      if (prev.forall(_.forall(_ == 0))) {
        end = true
      }

      if (buffer.length > nodes.length) {
        // primitive loop detection
        throw new GraphException("longest path exceeding # of nodes. Loop detected")
      }
    }

    buffer.toList
  }

  /**
   * finds all the paths between the two given nodes
   * Makes use of the adjacency matrix
   *
   */
  def pathsBetween(source: Cluster, target: Cluster): List[List[Cluster]] =
    {

      val n = nodes.length
      val s = node2Num(source)
      val t = node2Num(target)

      val P = Array.fill[Int](n)(-1)
      P(s) = -2
      var paths = ListBuffer[List[Cluster]]()

      visitPath(-2, s, t, P, paths)
      paths.toList
    }

  /**
   * a helper method used for pathsBetween
   *
   */
  private def visitPath(parent: Int, u: Int, t: Int, P: Array[Int], paths: ListBuffer[List[Cluster]]) {

    P(u) = parent
    if (u == t) {
      //record the path here
      var route = List[Int](u)
      var cur = u
      while (P(cur) != -2) {
        route = P(cur) :: route
        cur = P(cur)
      }
      paths += route.map(num2Node(_))
      //route = route.reverse
      //println("route = " + route.name))

    } else {
      // next node to visit
      val next = (0 until P.length).filter(adjacentMtx(u)(_) > 0)
      for (v <- next if P(v) == -1) {
        visitPath(u, v, t, P, paths)
      }
    }
    P(u) = -1
  }

  override def shortestDistance(source: Cluster, target: Cluster): Int =
    {
      var step = 0
      var found = false
      var list = adjList
      var s = node2Num(source)
      var t = node2Num(target)
      while (!found && list != Nil) {
        if (list.head(s)(t) > 0)
          found = true
        step += 1
        list = list.tail

      }

      if (!found)
        -1
      else step
    }

  override def diameter(source: Cluster, target: Cluster): Int =
    {
      var step = 0
      var found = false
      var list = adjList.reverse
      var s = node2Num(source)
      var t = node2Num(target)
      while (!found && list != Nil) {
        if (list.head(s)(t) > 0)
          found = true
        step += 1
        list = list.tail
      }

      if (!found)
        -1
      else (adjList.length - step + 1)
    }

  override def takeSteps(source: Cluster, steps: Int): List[Cluster] =
    {
      if (steps > adjList.length) return Nil

      var list = adjList
      var s = node2Num(source)
      val row = adjList(steps - 1)(s)

      val ids = (0 until nodes.length).filter(row(_) > 0) // column # of the matrix
      ids.map(num2Node(_)).toList
    }

  def numPaths(source: Cluster, sink: Cluster): Int =
    {
      val s = node2Num(source)
      val t = node2Num(sink)
      adjList.map(m => m(s)(t)).sum
    }

  /**
   * finds the source nodes in the graph.
   * A source node is defined as a node without incoming links and
   * the longest path starting from it is at least half as long
   * as the longest path in the graph
   *
   */
  def sourceNodes(): List[Int] = {
    val n = adjacentMtx.length
    val maxPath = adjList.length
    (0 until n).filter {
      ind =>
        val noIncoming = (0 until n).forall(adjacentMtx(_)(ind) == 0)

        val path = adjList.indexWhere { mtx => mtx(ind).forall(_ == 0) }
        //if (noIncoming) println("no incoming: " + ind + " path = " + path)
        noIncoming && (path > 0.5 * maxPath || path == -1)
    }.toList
  }

  //  def sourceClusters():List[Cluster] = {
  //    sourceNodes().map(num2Node(_))
  //  }

  /**
   * finds the sink nodes in the graph.
   * A sink node is defined as a node without outgoing links and
   * the longest path ending at it is at least half as long
   * as the longest path in the graph
   *
   */
  def sinkNodes(): List[Int] = {
    val n = adjacentMtx.length
    val maxPath = adjList.length
    (0 until n).filter {
      ind =>
        val noOutgoing = adjacentMtx(ind).forall(_ == 0)
        val path = adjList.indexWhere { mtx => (0 until n).forall { mtx(_)(ind) == 0 } }
        noOutgoing && (path > 0.5 * maxPath || path == -1)
    }.toList
  }

  //  def sinkClusters():List[Cluster] = {
  //    sourceCluster
  //  }

  /**
   * Find the minimum vertex cut of a graph by transforming the problem into a maximum
   * flow problem and a minimum edge-cut problem.
   *
   */
  def minVertexCut(): List[List[Cluster]] =
    {

      // we need to make the necessary data structures used by EdmondsKarp
      val n = nodes.length

      // First, we find out the source node and the sink node
      val sources = sourceNodes()
      val sinks = sinkNodes()
      //val source = sources.head
      //val sink = sinks.head
      if (sources == Nil || sinks == Nil) throw new RuntimeException()
      println("sources = " + sources)
      println("sinks = " + sinks)

      if (sources.length + sinks.length == n) return Nil // the graph contains nothing but sources and sinks. No cuts.
      if (sources.exists(s => sinks.exists(t => adjacentMtx(s)(t) > 0))) return Nil // there is a direct link from source to sink. No cuts.

      var newNodes = nodes
      var newLinks = links
      var source: Cluster = num2Node(sources.head)
      // when more than once source exists, we have to create a single new source that replaces the old sources
      if (sources.length > 1) {
        source = new Cluster("new source", Nil)
        // inherits all the outgoing links from all sources
        newLinks = newLinks.map {
          l =>
            if (sources.contains(node2Num(l.source)))
              new Link(source, l.target)
            else l
        }

        // replace the first source, and delete other sources
        newNodes = source :: newNodes -- sources.map(num2Node(_))
      }

      var sink: Cluster = num2Node(sinks.head)
      // when more than once sink exists, we have to create a single new sink that replaces the old sinks
      if (sinks.length > 1) {
        sink = new Cluster("new sink", Nil)
        // inherits all the outgoing links from all sources
        newLinks = newLinks.map {
          l =>
            if (sinks.contains(node2Num(l.target)))
              new Link(l.source, sink)
            else l
        }

        // replace the first source, and delete other sources
        newNodes = sink :: newNodes -- sinks.map(num2Node(_))
      }

      val reGraph = new EfficientGraph(newNodes, newLinks) // the graph with the new source and/or new sink
      //println(reGraph.num2Node.map { case (num, node) => num + " " + node.name + "\n" }.mkString)
      val capacity = getCapacity(reGraph)
      val neighbors = getNeighbors(capacity)
      var sId = reGraph.node2Num(source) * 2 + 1 // starting from the outgoing half of the source node
      val tId = reGraph.node2Num(sink) * 2 // reaching the incoming half of the source node

      //      (0 to 5).foreach { du =>
      //        println(du + " iteration ***********************************")
      val ans = EdmondsKarp.compute(capacity, neighbors, sId, tId)
      //println("flow = " + ans._1)
      //println("flow matrix: ")
      //utils.Matrix.prettyPrint(ans._2)
      val flow = ans._2
      //drawNumberedGraph(flow, "flow")
      //drawNumberedGraph(capacity, "capacity"+du)
      val cutEdges = realCutEdges(capacity, flow, sId, tId)
      //sId = cutEdges.head.head._2
      // converting from the cut edges in the reconstructed graph to the cut vertices in the original graph
      var cutNodes = cutEdges.map {
        _ map {
          case (a, b) =>
            val small = math.min(a, b)
            val big = math.max(a, b)
            /* all cut edge should start from an even-numbered node and end at an odd-numbered node
               These two nodes represent one node in the original graph
               If any other edges are saturated, raise an alarm.
             */
            if (small % 2 != 0) throw new GraphException("small = " + small + ". Errors in program!")

            reGraph.num2Node(small / 2)
        }
      }

      // filter out trivial cuts that are right next to the source or the sink

      cutNodes = cutNodes.filterNot(cut => (cut.forall(n => reGraph.shortestDistance(source, n) == 1) && cut.size >= sources.size) ||
        cut.forall(n => reGraph.shortestDistance(n, sink) == 1) && cut.size >= sinks.size)

      println(cutNodes.map(_.map(_.name)))
      //}
      //println("sat? " + capacity(20)(21))

      cutNodes.distinct
    }

  /**
   * Traverse the residue graph to find the minimum cut edges for a maximum flow
   * capacity: the capacity matrix
   * flow: the maximum flow matrix
   * s: source
   * t: sink
   *
   * returns a list of minimum sets of cut edges. Each set is a minimum edge cut.
   * This list should be exhaustive.
   */
  private def realCutEdges(capacity: Array[Array[Int]], flow: Array[Array[Int]], s: Int, t: Int): List[List[(Int, Int)]] =
    {
      val n = capacity.length
      val cutEdges = ListBuffer[List[(Int, Int)]]()
      var unreached = ListBuffer[Int]()
      val residue = Array.ofDim[Int](n, n)
      for (i <- 0 until n; j <- 0 until n) {
        residue(i)(j) = //capacity(i)(j) - flow(i)(j)
          if (capacity(i)(j) - flow(i)(j) > 0) 1 else if (capacity(i)(j) > 0) -1 else 0
        //        {
        //          if (capacity(i)(j) == 0) 0 //was not an edge before 
        //          else if (capacity(i)(j) > flow(i)(j)) 1 // some capacity remaining           
        //          else -1 // no capacity remaining because it is exhausted. 
        //        }
      }

      //find the underlying graph
      //      for (i <- 0 until n; j <- 0 until n) {
      //        if (residue(i)(j) == 1) {
      //          if (i % 2 == 1)
      //            residue(j)(i - 1) = 1
      //          else residue(j)(i) = 1
      //        }
      //      }

      //println("residue = ")
      //utils.Matrix.prettyPrint(residue)

      unreached ++= (0 until n by 2) // IMPORTANT: cannot start traversal on a odd node, except the source
      var x = traverse(residue, s)

      /* IMPORTANT:We can never start the traversal from a outgoing node when 
       * the link between the incoming node and the outgoing node is cut
       
      for (i <- 0 until n if (i % 2 == 0 && residue(i)(i + 1) == -1)) {
        unreached -= i
        unreached -= i + 1
      } */

      unreached --= x._1
      cutEdges += x._2
      if (!unreached.contains(t)) throw new GraphException("the graph is not completely cut")

      x = traverse(utils.Matrix.transpose(residue), t)
      unreached --= x._1
      cutEdges += x._2

      while (!unreached.isEmpty) {
        val u = unreached.head
        x = traverse(residue, u)
        unreached --= x._1
        cutEdges += x._2
      }

      cutEdges.filterNot(_ == Nil).distinct.toList
    }

  /**
   * Traversing the residue graph in a breath-first manner.
   * Saturated edges that prevent us from reach more points are cut edges.
   */
  private def traverse(residue: Array[Array[Int]], start: Int): (List[Int], List[(Int, Int)]) =
    {
      println("starting from " + start)
      val n = residue.length
      val parents = Array.fill[Int](n)(-1)
      parents(start) = -2
      var answer = List[(Int, Int)]()

      val q = scala.collection.mutable.Queue[Int]()
      q.enqueue(start)

      while (!q.isEmpty) {
        val u = q.dequeue
        //println("dequeued " + u)
        (0 until n).filter(residue(u)(_) > 0).foreach {
          v =>
            //println("visited " + v)
            if (v != u && parents(v) == -1) // v is not visited yet
            {
              q.enqueue(v)
              parents(v) = u
            }
        }

        // edges that cannot be crossed from u
        (0 until n).filter { residue(u)(_) < 0 }.foreach { i =>
          answer = (u, i) :: answer
        }
      }

      answer = answer.filter { case (a, b) => parents(a) == -1 || parents(b) == -1 }.distinct
      val reached = (0 until n).filter(parents(_) != -1).toList
      println("reached = " + reached + " answer = " + answer)
      (reached, answer)
    }

  private def getNeighbors(capacity: Array[Array[Int]]): Array[List[Int]] = {
    val n = capacity.length
    val neighbors = Array.fill[List[Int]](n)(List[Int]())
    for (i <- 0 until n; j <- 0 until n) {
      if (capacity(i)(j) > 0) {
        neighbors(i) = j :: neighbors(i)
        neighbors(j) = i :: neighbors(j)
      }
    }

    for (i <- 0 until n)
      neighbors(i) = neighbors(i).distinct

    neighbors
  }

  /**
   * Transforms the original graph so that the edge-cut algorithm can be used
   * to find vertex cuts.
   *
   * Each node in the original graph is converted two nodes, with the number 2n and 2n+1.
   * An edge of capacity 1 links the two nodes, from 2n to 2n+1
   * Edge cuts found should only contain such edges.
   */
  private def getCapacity(graph: EfficientGraph): Array[Array[Int]] = {
    val n = graph.nodes.length
    val capacity = Array.ofDim[Int](n * 2, n * 2)
    val infiniteCapacity = 1000
    val adjMtx = graph.adjacentMtx

    for (i <- 0 until n) {
      // this is the in-node. Its only out-link is to the out-node with capacity 1
      capacity(i * 2)(i * 2 + 1) = 1
      // in-links for the in-node
      (0 until n).filter(adjMtx(_)(i) > 0).foreach {
        ind =>
          capacity(ind * 2 + 1)(i * 2) = infiniteCapacity
      }

      // this is the out-node
      (0 until n).filter(adjMtx(i)(_) > 0).foreach {
        ind =>
          capacity(i * 2 + 1)(ind * 2) = infiniteCapacity
      }
    }
    //println("capacity: ")
    //utils.Matrix.prettyPrint(capacity)

    capacity
  }

  /**
   * produces the graph from the original sources to the given vertex cut
   *
   */
  def topCut(vertexCut: List[Cluster], mergedWith: collection.mutable.HashMap[List[Cluster], List[Cluster]]): EfficientGraph =
    {
      val source = sourceNodes().map(num2Node(_))
      val sink = if (mergedWith.contains(vertexCut)) (vertexCut ::: mergedWith(vertexCut)) else vertexCut
      interGraph(source, sink)
    }

  /**
   * produces graphs between cuts
   *
   */
  def middleCuts(vertexCuts: List[List[Cluster]], mergedWith: collection.mutable.HashMap[List[Cluster], List[Cluster]]): List[EfficientGraph] =
    {
      (vertexCuts zip vertexCuts.tail) map {
        case (cut1, cut2) =>
          var start = if (mergedWith.contains(cut1)) (cut1 ::: mergedWith(cut1)) else cut1
          var end = if (mergedWith.contains(cut2)) (cut2 ::: mergedWith(cut2)) else cut2
          interGraph(start, end)
      }
    }

  /**
   * produces the graph between the vertex cut and the original sinks
   *
   */
  def lastCut(vertexCut: List[Cluster], mergedWith: collection.mutable.HashMap[List[Cluster], List[Cluster]]): EfficientGraph =
    {
      val source = if (mergedWith.contains(vertexCut)) (vertexCut ::: mergedWith(vertexCut)) else vertexCut
      val sink = sinkNodes.map(num2Node(_))
      interGraph(source, sink)
    }

  /**
   * produces a new graph based on given sources and given sinks.
   * All links and nodes that appear on any path from any source to any sink is included.
   */
  private def interGraph(sources: List[Cluster], sinks: List[Cluster]): EfficientGraph = {
    var usedNodes = ListBuffer[Cluster]()

    // we capture all nodes on all paths from any source to any sink

    usedNodes ++=
      (for (s <- sources; t <- sinks) yield (s, t)).flatMap {
        case (s, t) =>
          pathsBetween(s, t)
      }.flatMap { x => x }.distinct
    //println("used nodes are: " + usedNodes)
    // links that involve used nodes only
    var newLinks = links.filter { l => usedNodes.contains(l.source) && usedNodes.contains(l.target) }

    if (sources.length > 1) {
      // create a new node that represent all source nodes.
      val newSource = new Cluster("new source", Nil)

      newLinks = newLinks.collect {
        // if the link goes out from a source, but does not end in a source, include it
        case l: Link if (sources.contains(l.source) && !sources.contains(l.target)) =>
          new Link(newSource, l.target)
        // if the link does not involve any sources, include it
        case l: Link if (!sources.contains(l.source) && !sources.contains(l.target)) =>
          l

        // the 3rd case here is that the link goes between sources. In that case, we discard that link.
      }

      // delete other sources, and add the new source
      usedNodes = usedNodes -- sources + newSource
    }

    // the same logic applied to sources also applies to sinks
    if (sinks.length > 1) {
      val newSink = new Cluster("new sink", Nil)
      newLinks = newLinks.collect {
        case l: Link if (sinks.contains(l.target) && !sinks.contains(l.source)) =>
          new Link(l.source, newSink)
        case l: Link if (!sinks.contains(l.source) && !sinks.contains(l.target)) =>
          l
      }

      usedNodes = usedNodes -- sinks + newSink
    }

    new EfficientGraph(usedNodes.toList, newLinks)
  }

  def drawNumberedGraph(matrix: Array[Array[Int]], name: String) {
    import java.io._
    val filename = name + ".txt"
    val file = new File(filename)
    val writer = new PrintWriter(new BufferedOutputStream(new FileOutputStream(file)))
    val used = (0 until matrix.length).filter {
      ind =>
        // exists outgoing or incoming links (whether saturated or not)
        matrix(ind).exists(_ != 0) ||
          (0 until matrix.length).exists(j => matrix(j)(ind) != 0)

    }
    writer.println("digraph G {")
    for (i <- 0 until used.length; j <- 0 until used.length) {
      if (matrix(used(i))(used(j)) > 0)
        writer.println(used(i) + " -> " + used(j) + "[style = \"dashed\"]")
      else if (matrix(used(i))(used(j)) < 0)
        writer.println(used(i) + " -> " + used(j))
    }
    writer.println("}")
    writer.close()

    val lastIndex = if (!filename.contains(".")) filename.length else filename.lastIndexOf(".")
    val outputName = filename.substring(0, lastIndex)
    Runtime.getRuntime().exec("dot -Tpng -o" + outputName + ".png " + filename)
    file.deleteOnExit()
    Thread.sleep(2000)
  }

  /**
   * Removes from the graph:
   *  1. nodes without predecessors but are not sources.
   *  2. nodes without successors but are not sinks
   */
  def removeIrregularSourceEnds(): EfficientGraph =
    {
      val realSource = sourceNodes().map(num2Node)
      val fakeSource = nodes.filter(n => links.forall(l => l.target != n)) -- realSource

      val realSinks = sinkNodes.map(num2Node)
      val fakeSinks = nodes.filter(n => links.forall(l => l.source != n)) -- realSinks

      val fakes = fakeSource ::: fakeSinks
      val realNodes = nodes -- fakes
      val realLinks = links.filterNot(l => fakes.contains(l.source) || fakes.contains(l.target))

      new EfficientGraph(realNodes, realLinks)
    }



  /**
   * removes nodes
   * Handles the optional case and the non-optional case
   */
//  def removeNodesWithOptionals(exclList: List[Cluster], optionals: List[Cluster]): EfficientGraph =
//    {
//      val options = exclList filter (optionals contains)
//      val excluded = exclList filterNot (optionals contains)
//
//      var graph = recursiveRemoveNodes(excluded, optionals)
//      graph = graph.removeNodes(options)
//      graph
//    }

//  /**
//   * Delete nodes from the graph. If all parents of a node are deleted, it will be deleted as well.
//   * When a node is deleted, its parent will directly connect to its children.
//   * This method is needed for generating stories from plot graphs.
//   *
//   */
//  def recursiveRemoveNodes(exclList: List[Cluster], optionals: List[Cluster]): EfficientGraph =
//    {
//      var excluded = ListBuffer[Cluster]() ++ exclList
//
//      var exclChildren = List[Cluster]()
//      do {
//        // If all parents of a node appear in excluded, the node should appear in exclChildren
//        exclChildren = (nodes filterNot (excluded contains)).filter {
//          n =>
//            val parents = predecessorsOf(n)
//            parents.size > 0 && (!(parents filterNot (optionals contains)).isEmpty) && // not all parents are optional AND
//              (parents forall (excluded contains)) // they are all contained in the excluded list
//        }
//        //println("excl children: " + exclChildren.map(_.name).mkString(", "))
//        excluded ++= exclChildren
//
//      } while (exclChildren != Nil)
//
//      var newLinks = ListBuffer[Link]() ++ links
//      excluded foreach {
//        c =>
//          val outLinks = links.filter(l => l.source == c)
//          val successors = outLinks.map(_.target)
//          val inLinks = links.filter(l => l.target == c)
//          val predecessors = inLinks.map(_.source)
//
//          newLinks --= outLinks
//          newLinks --= inLinks
//
//          for (a <- predecessors; b <- successors) {
//            newLinks += new Link(a, b)
//          }
//      }
//
//      newLinks = newLinks.filterNot(l => excluded.contains(l.source) || excluded.contains(l.target))
//      new EfficientGraph(nodes filterNot (excluded contains), newLinks.toList)
//    }

}

object EffciencyTest extends App {

  //  testVertexCut4()
  testPath2()

  def testPath1() {
    val g = SampleGraph.sample1.makeEfficient
    val c1 = g.nodes.find(_.name == "c1").get
    val c7 = g.nodes.find(_.name == "c7").get
    g.pathsBetween(c1, c7)
  }

  def testPath2() {
    val g = SampleGraph.sample2.makeEfficient
    val c1 = g.nodes.find(_.name == "c1").get
    val c8 = g.nodes.find(_.name == "c8").get
    g.pathsBetween(c1, c8)
  }

  def testVertexCut1() {
    val c1 = new Cluster("C1", Nil)
    val c2 = new Cluster("C2", Nil)
    val c3 = new Cluster("C3", Nil)
    val c4 = new Cluster("C4", Nil)
    val l1 = new Link(c1, c2)
    val l2 = new Link(c1, c3)
    val l3 = new Link(c3, c4)
    val l4 = new Link(c2, c4)

    val g = new EfficientGraph(List(c1, c2, c3, c4), List(l1, l2, l3, l4))
    g.minVertexCut()
  }

  // correct cut is (C2) (C6)
  def testVertexCut2() {
    val c1 = new Cluster("C1", Nil)
    val c2 = new Cluster("C2", Nil)
    val c3 = new Cluster("C3", Nil)
    val c4 = new Cluster("C4", Nil)
    val c5 = new Cluster("C5", Nil)
    val c6 = new Cluster("C6", Nil)
    val c7 = new Cluster("C7", Nil)
    val l1 = new Link(c1, c2)
    val l2 = new Link(c2, c3)
    val l3 = new Link(c2, c4)
    val l4 = new Link(c2, c5)
    val l5 = new Link(c5, c6)
    val l6 = new Link(c3, c6)
    val l7 = new Link(c4, c6)
    val l8 = new Link(c6, c7)

    val g = new EfficientGraph(List(c1, c2, c3, c4, c5, c6, c7), List(l1, l2, l3, l4, l5, l6, l7, l8))
    val answer = g.minVertexCut()
    println("cut nodes are " + answer.map(_.map(_.name).mkString("(", ", ", ")")).mkString(" "))
  }

  // currect cut is C3
  def testVertexCut3() {
    val c1 = new Cluster("C1", Nil)
    val c2 = new Cluster("C2", Nil)
    val c3 = new Cluster("C3", Nil)
    val c4 = new Cluster("C4", Nil)
    val c5 = new Cluster("C5", Nil)
    val c6 = new Cluster("C6", Nil)
    val c7 = new Cluster("C7", Nil)
    val c8 = new Cluster("C8", Nil)
    val c9 = new Cluster("C9", Nil)
    val links = List(new Link(c1, c3),
      new Link(c2, c3),
      new Link(c3, c4),
      new Link(c3, c5),
      new Link(c3, c6),
      new Link(c4, c7),
      new Link(c5, c7),
      //new Link(c6, c7),
      //new Link(c4, c8),
      new Link(c5, c8),
      new Link(c6, c8),
      new Link(c7, c9),
      new Link(c8, c9))

    val g = new EfficientGraph(List(c1, c2, c3, c4, c5, c6, c7, c8, c9), links)
    val answer = g.minVertexCut()
    println("cut nodes are " + answer.map(_.map(_.name).mkString("(", ", ", ")")).mkString(" "))
    println(g.numPaths(c2, c9))
  }

  // currect cut is (C7, C8)
  def testVertexCut4() {
    val c3 = new Cluster("C3", Nil)
    val c4 = new Cluster("C4", Nil)
    val c5 = new Cluster("C5", Nil)
    val c6 = new Cluster("C6", Nil)
    val c7 = new Cluster("C7", Nil)
    val c8 = new Cluster("C8", Nil)
    val c9 = new Cluster("C9", Nil)
    val links = List(
      new Link(c3, c4),
      new Link(c3, c5),
      new Link(c3, c6),
      new Link(c4, c7),
      new Link(c5, c7),
      new Link(c5, c8),
      new Link(c6, c8),
      new Link(c7, c8),
      new Link(c7, c9),
      new Link(c8, c9))

    val g = new EfficientGraph(List(c3, c4, c5, c6, c7, c8, c9), links)
    val answer = g.minVertexCut()
    println("cut nodes are " + answer.map(_.map(_.name).mkString("(", ", ", ")")).mkString(" "))
  }

  // currect cut is C8
  def testVertexCut5() {
    val c1 = new Cluster("C1", Nil)
    val c2 = new Cluster("C2", Nil)
    val c3 = new Cluster("C3", Nil)
    val c4 = new Cluster("C4", Nil)
    val c5 = new Cluster("C5", Nil)
    val c6 = new Cluster("C6", Nil)
    val c7 = new Cluster("C7", Nil)
    val c8 = new Cluster("C8", Nil)
    val c9 = new Cluster("C9", Nil)
    val c10 = new Cluster("C10", Nil)
    val links = List(new Link(c1, c2),
      new Link(c1, c3),
      new Link(c1, c4),
      new Link(c1, c5),
      new Link(c2, c6),
      new Link(c3, c6),
      new Link(c5, c6),
      new Link(c4, c7),
      new Link(c3, c7),
      new Link(c6, c7),
      new Link(c6, c8),
      new Link(c7, c8),
      new Link(c8, c9),
      new Link(c8, c10))

    val g = new EfficientGraph(List(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10), links)
    val answer = g.minVertexCut()
    println("cut nodes are " + answer.map(_.map(_.name).mkString("(", ", ", ")")).mkString(" "))
  }

  def testCase1() {
    val c1 = new Cluster("C1", Nil)
    val c2 = new Cluster("C2", Nil)
    val c3 = new Cluster("C3", Nil)
    val c4 = new Cluster("C4", Nil)
    val c5 = new Cluster("C5", Nil)
    val c6 = new Cluster("C6", Nil)
    val c7 = new Cluster("C7", Nil)
    val c8 = new Cluster("C8", Nil)

    val l1 = new Link(c1, c2)
    val l2 = new Link(c2, c3)
    val l3 = new Link(c3, c4)
    val l4 = new Link(c4, c5)
    val l5 = new Link(c2, c5)
    val l6 = new Link(c5, c6)
    val l7 = new Link(c6, c7)
    val l8 = new Link(c7, c8)
    val l9 = new Link(c4, c8)
    val l10 = new Link(c2, c8)
    val l11 = new Link(c8, c1)

    val g = new EfficientGraph(List(c1, c2, c3, c4, c5, c6, c7, c8), List(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10))
    val g2 = new Graph(List(c1, c2, c3, c4, c5, c6, c7, c8), List(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10))
    //  println(g.shortestDistance(c1, c3))
    //  println(g.shortestDistance(c4, c3))
    //  println(g.shortestDistance(c1, c4))
    var time = System.currentTimeMillis()
    for (i <- 0 to 10000) {
      g.shortestDistance(c1, c8)
      g.diameter(c1, c8)
      g.takeSteps(c1, 2)
    }

    println(System.currentTimeMillis() - time)

    time = System.currentTimeMillis()
    for (i <- 0 to 10000) {
      g2.shortestDistance(c1, c8)
      g2.diameter(c1, c8)
      g2.takeSteps(c1, 2)
    }

    println(System.currentTimeMillis() - time)
  }

}