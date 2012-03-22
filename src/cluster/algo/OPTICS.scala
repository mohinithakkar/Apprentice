package cluster.algo
import data._
import parse._
import scala.collection.mutable.PriorityQueue
import scala.math.Ordered
object OPTICS {

  final val MAX_DISTANCE = Double.PositiveInfinity
  final val UNDEFINED: Double = -1

  case class Point(val id: Int) extends Ordered[Point] {
    var visited: Boolean = false
    var core: Double = -2
    var reachability: Double = UNDEFINED

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
    val text = clusterList.filter(_.members.length > 1).map(_.toHexSeparatedString()).mkString
    println()

    println(text)
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

    interpret(list.toArray)

    var clusterList = List[Cluster]()
    var curList: List[Sentence] = null
    // now we need to extract the clusters
    for (p <- list) {
      println(p.id + "/" + sentences(p.id).id + ": " + p.reachability)
      if (p.reachability == UNDEFINED) {
        //println("starting a cluster with " + p.id + " " + sentences(p.id).id)
        if (curList != null)
          clusterList = new Cluster("name", curList) :: clusterList
        curList = List[Sentence](sentences(p.id))
      } else {
        //println("adding to cluster " + p.id + " " + sentences(p.id).id)
        curList = sentences(p.id) :: curList
      }
    }

    clusterList = new Cluster("name", curList) :: clusterList

    clusterList
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

  class Node(val start: Int, val end: Int, var Parent: Node = null) {
    var children: List[Node] = List[Node]()
  }

  def interpret(array: Array[Point]): Node =
    {
      import scala.collection.mutable.ListBuffer

      var localMaxima = new ListBuffer[(Int, Point)]()

      val minClusterSize = scala.math.ceil(0.005 * array.length)
      println("minimum size = " + minClusterSize)
      // first, find all local maximum
      for (i <- 0 until array.length) {
        val point = array(i)
        val left = math.max(0, i - minClusterSize).toInt
        val right = math.min(array.length - 1, i + minClusterSize).toInt
        var (nbLeft, nbRight) = array.drop(left).dropRight(array.length - 1 - right).splitAt(i - left)
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
                if (array(i).reachability == UNDEFINED) true
                else p.reachability != UNDEFINED && p.reachability <= array(i).reachability
              }
          }

        if (maxima) {
          localMaxima += ((i, point))
          //println("****\n" + (nbLeft ++ nbRight).map(_.reachability).mkString("\n"))
          //println("lm: " + array(i).reachability + "****\n")
        }
      }

      val lm = localMaxima.sortBy(_._2).toList
      
      //println(lm.map(_._2.reachability).mkString("\n"))
      val root = new Node(0, array.length - 1)
      clusterTree(root, null, lm.map(_._1), array)
      System.exit(-1)
      root
    }

  def clusterTree(n: Node, parent: Node, maxima: List[Int], points: Array[Point]) {

  }

}