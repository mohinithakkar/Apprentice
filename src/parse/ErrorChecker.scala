package parse

import data._
import graph._
import scala.collection.mutable.HashMap

/** This is the first implementation
 *
 */
class ErrorChecker {

  // (source, target) -> (expected, real)
  var hash = new HashMap[Link, (Double, Double)]()
  var storyDistances: HashMap[(Cluster, Cluster), Double] = HashMap()

  def getGoodPaths(): List[(Link, (Double, Double))] =
    {
      hash.filter {
        x =>
          val (expected, real) = x._2
          math.abs(expected - real) < 1
      }.toList
    }

  def getBadPaths(): List[(Link, (Double, Double))] =
    {
      hash.filter {
        x =>
          val (expected, real) = x._2
          //Math.abs(expected - real) > 2
          //expected - real > 2

          if (real != 0) expected - real > 0 // there are links
          else math.abs(expected) > 0 // there are no links. i.e. parallel
      }.toList
    }

  def checkErrors(storyList: List[Story], simpleGraph: Graph) = {

    val usedClusters = simpleGraph.usedClusters
    // this avoids computing the story based distances again
    if (storyDistances.isEmpty)
      storyDistances = storyBasedDistance(storyList)
    compareDist(storyDistances, simpleGraph)
  }

  /** computer the average distances over the stories */
  def storyBasedDistance(storyList: List[Story]): HashMap[(Cluster, Cluster), Double] =
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
              }
              else
                addDistance(source, target, j - i)
            }
          }
      }

      val rtn =
        distanceTable.map { e =>
          val distance = e._2._1.toDouble / e._2._2
          e._1 -> distance
        }
    }

  def compareDist(distances: HashMap[(Cluster, Cluster), Double], graph: Graph): (Double, Double) = {
    var sum: Double = 0
    var total: Int = 0

    // (source, target) -> (expected, real) 
    // must create a new instance here.
    hash = new HashMap[Link, (Double, Double)]()
    // this list contains pairs of cluster that are parallel and are already counted
    var checkedList: List[(Cluster, Cluster)] = Nil
    // println("comparing distance")
    for (
      ((source, target), distance) <- distances if (graph.nodes.contains(source) && graph.nodes.contains(target))
    ) {
      var debug = false
      //debug = (source.name == "wait in line" && target.name == "choose menu item")
      //println("pair: " + source.name + ", " + target.name + ": " + distance)
      val forwardDist = graph.shortestDistance(source, target)

      if (debug)
        println("forward dist " + forwardDist)

      if (forwardDist == -1) {
        // forward link does not exist. But may the backward link exists
        val backwardDist = graph.shortestDistance(target, source)
        if (debug) println("backward dist " + backwardDist)
        if (backwardDist != -1) {
          //println("ignoring pair: " + source.name + ", " + target.name)
          // the backward link exists. We can safely ignore this pair of nodes
        }
        else {
          /* neither links exists. Therefore those nodes are parallel
              * First, check if the reverse has already been computed.
              * If not, sum up Dn from both directions and compare it with this pair's Dg, which is 0
              * Finally, add it to the checked list
              */
          if (!checkedList.contains((source, target))) {
            val reverseDistance = distances.getOrElse((target, source), 0.0)
            //println("reverse direction distance = " + reverseDistance)

            val totalDist = distance - reverseDistance
            sum += totalDist * totalDist
            total += 1

            val key = new Link(source, target)
            if (hash.get(key).isDefined) throw new Exception("should not contain " + source.name + ", " + target.name)
            hash += (key -> (totalDist, 0))
            checkedList = (source, target) :: checkedList
          }
        }
      }
      else {
        // found the forward link
        sum += (forwardDist - distance) * (forwardDist - distance)
        total += 1
        //          println(source.name + " -> " + target.name + ", expected: " + distance + " real: " + forwardDist)
        val key = new Link(source, target)
        if (hash.get(key).isDefined) throw new Exception("should not contain " + source.name + ", " + target.name)
        hash += (key -> (distance, forwardDist))
      }
    }
    val error = sum
    val avg = error / total
    //println("sum squared = " + error + ", avg = " + avg + " number of pairs = " + total)
    (error, avg)
  }

}