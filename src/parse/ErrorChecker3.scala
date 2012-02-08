package parse
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

/**
 * This error measure is mathematically sound.
 *
 */
class ErrorChecker3 {

  // (source, target) -> (expected, real)
  val hash = new HashMap[Link, (Double, Double)]()

  def getGoodPaths(): List[(Link, (Double, Double))] =
    {
      hash.filter {
        x =>
          val (expected, real) = x._2
          Math.abs(expected - real) < 1
      }.toList
    }

  def getBadPaths(): List[(Link, (Double, Double))] =
    {
      hash.filter {
        x =>
          val (expected, real) = x._2
          if (real != 0) expected - real > 2 // there are links
          else Math.abs(expected) > 2 // there are no links. i.e. parallel
      }.toList
    }

  def checkErrors(storyList: List[Story], clusterList: List[Cluster], reducedLinks: List[Link]) = {

    val usedClusters = reducedLinks flatMap { l => List(l.source, l.target) } distinct
    var distances = computeDistance(storyList, clusterList)
    compareDist(usedClusters, distances, reducedLinks)
  }

  /** computer the average distances over the stories */
  def computeDistance(storyList: List[Story], clusterList: List[Cluster]): HashMap[(Cluster, Cluster), (Int, Int)] =
    {
      // (sum, total)
      val distanceTable = new HashMap[(Cluster, Cluster), (Int, Int)]

      def addDistance(c1: Cluster, c2: Cluster, distance: Int) {
        var value = distanceTable.get((c1, c2)).getOrElse((0, 0))
        value = (value._1 + distance, value._2 + 1)
        distanceTable += (((c1, c2), value))
      }

      storyList foreach {
        story =>
          for (i <- 0 to story.members.length - 1) {
            for (j <- i + 1 to story.members.length - 1) {
              val source = story.members(i).cluster
              val target = story.members(j).cluster
              if (source == null || target == null) println(story + ", " + i + ", " + j + " have no clusters")
              if (source == target) {
                /*
            	  println("WARNING: SOURCE = " + source + "TARGET = " + target)
            	  println(story.members(i))
            	  println(story.members(j))*/
              } else
                addDistance(source, target, j - i)
            }
          }
      }

      val keys = distanceTable.keySet

      val distList = keys map {
        key =>
          val data = distanceTable.get(key).get
          val avgDist = data._1 / data._2.toDouble
          (key._1, key._2, avgDist)
      }

      //println(distList.toList)
      
      distanceTable
    }

  def compareDist(allClusters: List[Cluster], hashTable:HashMap[(Cluster, Cluster), (Int, Int)], links: List[Link]): (Double, Double) = {
    var sum: Double = 0
    var total: Int = 0

    // first, find all pairs (e1, e2) such that there is a path from e1 to e2
    //val queue = scala.collection.mutable.Queue[(Cluster, Int)]()
    var validPairs = new ListBuffer[(Cluster, Cluster, Int)]()
    //println("hello")
    for (i <- 0 to allClusters.length - 1)
      for (j <- i + 1 to allClusters.length - 1) {
        val source = allClusters(i)
        val target = allClusters(j)
        
        val forward = findShortestDistance(links, source, target)
        val backward = findShortestDistance(links, target, source)
        //println("computed " + source + "to" + target)
        if (forward != -1) {
          // there is a path going from source to target
          validPairs += ((source, target, forward))
        } else if (backward != -1) {
          // there is a path going from target to source
          validPairs += ((target, source, backward))
        } else {
          // target and source are parallel
          validPairs += ((source, target, 0))
        }
      }

    validPairs.toList foreach { x =>
      val source = x._1
      val target = x._2
      val dg = x._3
      
      val forward = hashTable.get((source, target)).getOrElse((0, 0))
      val backward = hashTable.get((target, source)).getOrElse((0, 0))
      
      //println(forward + " " + backward )
      
      val numerator = forward._1 - backward._1
      val denominator = forward._2 + backward._2
      //println(numerator + " / " + denominator)
      val dn = 
        if (denominator == 0) 0.0 // avoiding divide by zero
        else numerator / denominator.toDouble
        
      val difference = dn - dg // dn is expected and dg is actual
      sum += difference * difference
      total += 1
      hash.put(new Link(source, target), (dn, dg))
    }

    val error = sum
    val avg = error / total
    println("sum squared = " + error + ", avg = " + avg + " number of pairs = " + total)
    (error, avg)
  }

  /**
   * find the shortest part distance between the two nodes based on the graph
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