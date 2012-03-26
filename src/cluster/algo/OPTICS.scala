package cluster.algo
import data._
import parse._
import scala.collection.mutable.PriorityQueue
import scala.math.Ordered
object OPTICS {

  final val MAX_DISTANCE = Double.PositiveInfinity
  final val UNDEFINED: Double = Double.PositiveInfinity
  var minClusterSize: Int = 1

  case class Point(val id: Int) extends Ordered[Point] {
    var visited: Boolean = false
    var core: Double = -2
    var reachability: Double = UNDEFINED
    var localMaximum = false
    var plotId = 0

    override def compare(that: Point): Int = {
      if (this.reachability == UNDEFINED && that.reachability != UNDEFINED) -1 // UNDEFINED is greater than everthing
      else if (this.reachability != UNDEFINED && that.reachability == UNDEFINED) 1
      else if (this.reachability < that.reachability) 1
      else if (this.reachability == that.reachability) 0
      else -1
    }
  }

  def main(args: Array[String]) {
    var matrix = HierarchicalClusterer.readCSV("movieOriginalSimilarity.csv")
    var sentences: List[Sentence] = SimpleParser.parseStories("./data/movie/movieSimpleStories.txt").flatMap(_.members).toList
    var max: Double = 0
    val distance = matrix.map { a =>
      a.map { value =>
        if (value != 0) {
          val v = 10 / value
          if (v > max) max = v
          v
        } else Double.PositiveInfinity
      }
    }

    val clusterList = cluster(distance, max, 4, sentences)
    //val text = clusterList.filter(_.members.length > 1).map(_.toHexSeparatedString()).mkString
    //println()

    //println(text)
  }

  def cluster(similarity: Array[Array[Double]], epsilon: Double, minPts: Int, sentences: List[Sentence]): List[Cluster] = {

    val length = similarity.length
    val points = (0 until length).map(new Point(_)).toArray
    for (p <- points) p.core = computeCore(p, epsilon, minPts, similarity)
    /*for each point p of DB
       p.reachability-distance = UNDEFINED
    */
    var ordered = List[Point]()

    // for each unprocessed point p of DB
    for (i <- 0 until length)
      if (!points(i).visited) {
        val p = points(i)
        //println("processing " + p.id)
        // N = getNeighbors(p, eps)
        val neighbors = getNeighbors(p, epsilon, similarity, points)
        //println("neighbors are: " + neighbors)
        // mark p as processed
        p.visited = true

        // output p to the ordered list
        ordered = p :: ordered

        /*
         * Seeds = empty priority queue
	       if (core-distance(p, eps, Minpts) != UNDEFINED)
	          update(N, p, Seeds, eps, Minpts)
	          for each next q in Seeds
	             N' = getNeighbors(q, eps)
	             mark q as processed
	             output q to the ordered list
	             if (core-distance(q, eps, Minpts) != UNDEFINED)
	                update(N', q, Seeds, eps, Minpts)
          */

        // Seeds = empty priority queue
        var seeds = new PriorityQueue[Point]()
        // if (core-distance(p, eps, Minpts) != UNDEFINED)
        if (p.core != UNDEFINED) {
          seeds = update(neighbors, p, seeds, epsilon, minPts, similarity)
          while (!seeds.isEmpty) {
            val q = seeds.dequeue()
            val qNeighbors = getNeighbors(q, epsilon, similarity, points)
            q.visited = true
            ordered = q :: ordered
            if (q.core != UNDEFINED)
              seeds = update(qNeighbors, q, seeds, epsilon, minPts, similarity)
          }
        }
      }

    val list = ordered.reverse

    for (p <- list) {
     println("***" + p.id + "/" + sentences(p.id).id + ": " + p.reachability)
     println(sentences(p.id))
    }
//    val root = interpret(list.toArray)
//
//    var leaves = List[Node]()
//
//    def findLeaves(r: Node) {
//      if (r.children == Nil) leaves = r :: leaves
//      else r.children.foreach(findLeaves(_))
//    }
//
//    findLeaves(root)
//
//    leaves.foreach { leaf =>
//      println(leaf.rArray.map(pt => sentences(pt.id).toShortString()).mkString("\n"))
//      println("###")
    //}
    //    var clusterList = List[Cluster]()
    //    var curList: List[Sentence] = null
    //    // now we need to extract the clusters
    //    for (p <- list) {
    //      println(p.id + "/" + sentences(p.id).id + ": " + p.reachability)
    //      if (p.reachability == UNDEFINED) {
    //        //println("starting a cluster with " + p.id + " " + sentences(p.id).id)
    //        if (curList != null)
    //          clusterList = new Cluster("name", curList) :: clusterList
    //        curList = List[Sentence](sentences(p.id))
    //      } else {
    //        //println("adding to cluster " + p.id + " " + sentences(p.id).id)
    //        curList = sentences(p.id) :: curList
    //      }
    //    }
    //
    //    clusterList = new Cluster("name", curList) :: clusterList
    //
    //    clusterList

    Nil
  }

  protected def update(neighbors: List[Point], p: Point, seeds: PriorityQueue[Point], eps: Double, minPts: Int, similarity: Array[Array[Double]]): PriorityQueue[Point] = {
    /*coredist = core-distance(p, eps, MinPts)
    for each o in N
       if (o is not processed)
          new-reach-dist = max(coredist, dist(p,o))
          if (o.reachability-distance == UNDEFINED) // o is not in Seeds
              o.reachability-distance = new-reach-dist
              Seeds.insert(o, new-reach-dist)
          else               // o in Seeds, check for improvement
              if (new-reach-dist < o.reachability-distance)
                 o.reachability-distance = new-reach-dist
                 Seeds.move-up(o, new-reach-dist)
                 */

    var nQueue = seeds.clone
    val coreDist = p.core
    for (o <- neighbors)
      if (!o.visited) {
        //println("processing neighbor of " + p.id + " : " + o.id)
        val newReachDist = math.max(coreDist, similarity(p.id)(o.id))
        if (o.reachability == UNDEFINED) {
          //println(o.id + " new reachabitlity = " + newReachDist)
          o.reachability = newReachDist
          nQueue += o
        } else if (newReachDist < o.reachability) {
          o.reachability = newReachDist
          //println(o.id + " new reachabitlity = " + newReachDist)
          nQueue = (nQueue filterNot { _ == o }) += o
        }
      }
    nQueue
  }

  //  protected def coreDistance(p: Point, eps: Double, minPts: Int, similarity: Array[Array[Double]]): Double =
  //    {
  //      if (p.core == -2) {
  //        val i = p.id
  //        val sorted = similarity(i).sortWith(_ < _)
  //        // check if there are minPts within epsilon
  //        if (sorted(minPts - 1) > eps)
  //          p.core = -1
  //        else
  //          p.core = sorted(minPts - 1)
  //      }
  //      p.core
  //    }

  protected def computeCore(p: Point, eps: Double, minPts: Int, similarity: Array[Array[Double]]): Double =
    {
      val i = p.id
      val sorted = similarity(i).sortWith(_ < _)
      // check if there are minPts within epsilon
      if (sorted(minPts - 1) > eps)
        UNDEFINED
      else
        sorted(minPts - 1)
    }

  protected def getNeighbors(p: Point, eps: Double, similarity: Array[Array[Double]], points: Array[Point]): List[Point] =
    {
      points filter { i: Point => similarity(p.id)(i.id) < eps } toList
    }

  class Node(var localMaxima: List[Point], val rArray: Array[Point], var Parent: Node = null) {
    var children: List[Node] = List[Node]()
    var splitPt: Point = null
  }

  def interpret(rArray: Array[Point]): Node =
    {
      import scala.collection.mutable.ListBuffer

      for (i <- 0 until rArray.length)
        rArray(i).plotId = i

      var localMaxima = new ListBuffer[(Int, Point)]()

      minClusterSize = scala.math.ceil(0.005 * rArray.length).toInt
      println("minimum size = " + minClusterSize)
      // first, find all local maximum
      for (idx <- 0 until rArray.length) {
        val point = rArray(idx)
        val left = math.max(0, idx - minClusterSize).toInt
        val right = math.min(rArray.length - 1, idx + minClusterSize).toInt
        var (nbLeft, nbRight) = rArray.drop(left).dropRight(rArray.length - 1 - right).splitAt(idx - left)
        nbRight = nbRight.drop(1)

        val leftDefined = {
          if (!nbLeft.isEmpty) point.reachability != nbLeft.last.reachability
          else true
        }
        val rightDefined = {
          if (!nbRight.isEmpty) point.reachability != nbRight.head.reachability
          else true
        }

        val maxima = (rightDefined || leftDefined) &&
          (nbLeft ++ nbRight).foldLeft(true) { (b: Boolean, p: Point) =>
            b &&
              {
                if (rArray(idx).reachability == UNDEFINED) true // undefined points are always local maxima
                else p.reachability != UNDEFINED && p.reachability <= rArray(idx).reachability
              }
          }

        if (maxima) {
          localMaxima += ((idx, point))
          point.localMaximum = true
          //println("****\n" + (nbLeft ++ nbRight).map(_.reachability).mkString("\n"))
          //println("lm: " + array(i).reachability + "****\n")
        }
      }

      val lm = localMaxima.sortBy(_._2).toList

      println(lm.map(p => p._2.plotId + " " + p._2.reachability).mkString("\n"))
      val real = lm.map(_._2)
      val root = new Node(real, rArray, null)
      clusterTree(root, null)
      //System.exit(-1)
      
//      val buffer = ListBuffer[Node]()
//      var start = 0
//      for(x <- localMaxima.tail)
//      {
//        val end = x._1
//        val interval = (start until end).map(rArray(_)).toArray
//        val n = new Node(Nil, interval, root)
//        root.children = n :: root.children 
//        start = end
//      }
      
      root
    }

  def clusterTree(n: Node, parent: Node) {
    import scala.collection.mutable.ListBuffer

    println("starting collection")
    if (n.localMaxima.isEmpty) {
      println("no local maxima in this region")
      return
    }

    n.splitPt = n.localMaxima.head

    val rArray = n.rArray // reachability array
    var maxima = n.localMaxima.tail
    val split = rArray.findIndexOf(_ == n.splitPt)
    val splitPt = n.splitPt
    n.localMaxima = n.localMaxima.tail

    println("split pt = " + splitPt + " " + splitPt.plotId + " split = " + split)
    // cannot use the names N1, N2. Bug?
    var (n1, n2) = n.rArray.splitAt(split)
    //n2 = n2.drop(1)

    var L1 = new ListBuffer[Point]()
    var L2 = new ListBuffer[Point]()
    for (pt <- maxima) {
      if (pt.plotId < splitPt.plotId) L1 += pt
      else if (pt.plotId > splitPt.plotId) L2 += pt
    }

    println("L1 " + L1.map(_.plotId).mkString(", "))
    println("L2 " + L2.map(_.plotId).mkString(", "))

    val kid1 = new Node(L1.toList, n1)
    val kid2 = new Node(L2.toList, n2)
    val kids = ListBuffer(kid1, kid2)

    // average reachability of the left region
    val leftAvg = {
      // the next local maximum on the left
      //val left = (split - 1 to 0 by -1).find {i => reachPlot(i).localMaximum && reachPlot(i).reachability > splitPt.reachability * 0.9}.getOrElse(0)
      //val region = (left to split - 1).map(rArray(_).reachability)
      val region = (0 until split).map(rArray(_).reachability).filterNot(_ == UNDEFINED)
      // take care of UNDEFINED values
      //if (region.contains(UNDEFINED)) UNDEFINED
      //else 
      if (region.length > 0)
        region.sum / region.length
      else UNDEFINED
    }

    // average reachability of the right region
    val rightAvg = {
      // the next local maximum on the right
      //val right = (split + 1 until rArray.length).find { i => rArray(i).localMaximum && rArray(i).reachability > splitPt.reachability * 0.9}.getOrElse(points.length - 1)
      //val region = (split + 1 to right).map(rArray(_).reachability)
      val region = (split + 1 until rArray.length).map(rArray(_).reachability).filterNot(_ == UNDEFINED)
      // take care of UNDEFINED values
      // if (region.contains(UNDEFINED)) UNDEFINED
      //else 
      if (region.length > 0)
        region.sum / region.length
      else UNDEFINED
    }
    println(" regions left = " + leftAvg + ", right = " + rightAvg + ", threshold = " + splitPt.reachability * 0.85)
    // signficance == leftAvg and rightAvg < 75% of split point
    if (leftAvg > splitPt.reachability * 0.85 && rightAvg > splitPt.reachability * 0.85) {
      // this is not significant. Continue to the next
      println("not significant: " + leftAvg + ", " + rightAvg + " > " + splitPt.reachability * 0.85)
      if (!n.localMaxima.isEmpty) {
        n.splitPt = n.localMaxima.head
        n.localMaxima = n.localMaxima.tail
        clusterTree(n, parent)
      }
    } else {
      // remove clusters that are too small
      if (n1.length < minClusterSize) {
        println("kid 1 too small")
        kids -= kid1
      }
      if (n2.length < minClusterSize) {
        println("kid 2 too small")
        kids -= kid2
      }
      if (kids.isEmpty) {
        println("both kids too small")
        parent.children = n :: parent.children
        return
      }

      val nextParent = {
        if (parent != null && approximateHeight(n, parent)) {
          println("using parent as parent")
          parent
        } else {
          if (parent != null)
            parent.children = n :: parent.children
          println("using current node as parent")
          n
        }
      }

      for (kid <- kids) {
        println("going to kid " + kid.rArray.head.plotId + " to " + kid.rArray.last.plotId)
        clusterTree(kid, nextParent)
      }
    }
  }

  // TODO
  def approximateHeight(n: Node, parent: Node): Boolean = {
    import utils.Calculator
    val c = new Calculator()
    val v = parent.rArray.map(_.reachability)
    c.add(v)
    val stdDev = c.stdDev()

    math.abs(n.splitPt.reachability - parent.splitPt.reachability) < stdDev
  }

}