package parse
//import scala.collection.mutable.HashMap
//import data._
///**
// * This error measure is mathematically sound.
// *
// */
//class ErrorChecker2 {
//
//  def getNewInstance() = new ErrorChecker2()
//  // (source, target) -> (expected, real)
//  val hash = new HashMap[Link, (Double, Double)]()
//
//  def getGoodPaths(): List[(Link, (Double, Double))] =
//    {
//      hash.filter {
//        x =>
//          val (expected, real) = x._2
//          math.abs(expected - real) < 1
//      }.toList
//    }
//
//  def getBadPaths(): List[(Link, (Double, Double))] =
//    {
//      hash.filter {
//        x =>
//          val (expected, real) = x._2
//          //math.abs(expected - real) > 2
//          // expected - real > 2
//          
//          if (real != 0) expected - real > 0 // there are links
//          else math.abs(expected) > 0 // there are no links. i.e. parallel
//          
//      }.toList
//    }
//
//  def checkErrors(storyList: List[Story], clusterList: List[Cluster], reducedLinks: List[Link]) = {
//
//    val usedClusters = reducedLinks flatMap { l => List(l.source, l.target) } distinct
//    var distances = computeDistance(storyList, clusterList) filter { x => usedClusters.contains(x._1) && usedClusters.contains(x._2) }
//    compareDist(distances, reducedLinks)
//  }
//
//  /** computer the average distances over the stories */
//  def computeDistance(storyList: List[Story], clusterList: List[Cluster]): List[(Cluster, Cluster, Double)] =
//    {
//      // (sum, total)
//      val distanceTable = new HashMap[(Cluster, Cluster), (Int, Int)]
//
//      def addDistance(c1: Cluster, c2: Cluster, distance: Int) {
//        var value = distanceTable.get((c1, c2)).getOrElse((0, 0))
//        value = (value._1 + distance, value._2 + 1)
//        distanceTable += (((c1, c2), value))
//      }
//
//      storyList foreach {
//        story =>
//          for (i <- 0 to story.members.length - 1) {
//            for (j <- i + 1 to story.members.length - 1) {
//              val source = story.members(i).cluster
//              val target = story.members(j).cluster
//              //if (source == null || target == null) println(story + ", " + i + ", " + j + " have no clusters")
//              if (source == target) {
//                /*
//            	  println("WARNING: SOURCE = " + source + "TARGET = " + target)
//            	  println(story.members(i))
//            	  println(story.members(j))*/
//              } else
//                addDistance(source, target, j - i)
//            }
//          }
//      }
//
//      val keys = distanceTable.keySet
//
//      val distList = keys map {
//        key =>
//          val data = distanceTable.get(key).get
//          val avgDist = data._1 / data._2.toDouble
//          (key._1, key._2, avgDist)
//      }
//
//      distList.toList
//    }
//
//  def compareDist(distances: List[(Cluster, Cluster, Double)], links: List[Link]): (Double, Double) = {
//    var sum: Double = 0
//    var total: Int = 0
//
//    // this list contains pairs of cluster that are parallel and are already counted
//    var checkedList: List[(Cluster, Cluster)] = Nil
//
//    distances.foreach {
//      x =>
//        val source = x._1
//        val target = x._2
//        val distance = x._3
//
//        var debug = false
//        //debug = (source.name == "wait in line" && target.name == "choose menu item")
//        //println("pair: " + source.name + ", " + target.name)
//        val forwardDist = findShortestDistance(links, source, target)
//        
//        if (debug) 
//          println("forward dist " + forwardDist)
//        
//        if (forwardDist == -1) {
//          // forward link does not exist. But may the backward link exists
//          val backwardDist = findShortestDistance(links, target, source)
//          if (debug) println("backward dist " + backwardDist)
//          if (backwardDist != -1) {
//            //println("ignoring pair: " + source.name + ", " + target.name)
//            // the backward link exists. We can safely ignore this pair of nodes
//          } else {
//            /* neither links exists. Therefore those nodes are parallel
//              * First, check if the reverse has already been computed.
//              * If not, sum up Dn from both directions and compare it with this pair's Dg, which is 0
//              * Finally, add it to the checked list
//              */
//            if (!checkedList.contains((source, target))) {
//              val reverseDistance = distances.find(x => x._1 == target && x._2 == source).map(_._3).getOrElse(0.0)
//              //println("reverse direction distance = " + reverseDistance)
//
//              val totalDist = distance - reverseDistance
//              sum += totalDist * totalDist
//              total += 1
//              
//              if (hash.get(new Link(source, target)).isDefined) throw new Exception("should not contain " + source.name + ", " + target.name)
//              hash.put(new Link(source, target), (totalDist, 0))
//              checkedList = (source, target) :: checkedList
//            }
//          }
//        } else {
//          // found the forward link
//          sum += (forwardDist - distance) * (forwardDist - distance)
//          total += 1
//          //          println(source.name + " -> " + target.name + ", expected: " + distance + " real: " + forwardDist)
//          if (hash.get(new Link(source, target)).isDefined) throw new Exception("should not contain " + source.name + ", " + target.name)
//          hash.put(new Link(source, target), (distance, forwardDist))
//        }
//
//    }
//    val error = sum
//    val avg = error / total
//    //println("sum squared = " + error + ", avg = " + avg + " number of pairs = " + total)
//    (error, avg)
//  }
//
//
//}