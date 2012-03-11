package parse
//import scala.collection.mutable.HashMap
//import scala.collection.mutable.ListBuffer
//import data._
///**
// * This error measure is mathematically sound.
// *
// */
//class ErrorChecker3 {
// 
//  def compareDist(allClusters: List[Cluster], hashTable:HashMap[(Cluster, Cluster), (Int, Int)], links: List[Link]): (Double, Double) = {
//    var sum: Double = 0
//    var total: Int = 0
//
//    // first, find all pairs (e1, e2) such that there is a path from e1 to e2
//    //val queue = scala.collection.mutable.Queue[(Cluster, Int)]()
//    var validPairs = new ListBuffer[(Cluster, Cluster, Int)]()
//    //println("hello")
//    for (i <- 0 to allClusters.length - 1)
//      for (j <- i + 1 to allClusters.length - 1) {
//        val source = allClusters(i)
//        val target = allClusters(j)
//        
//        val forward = findShortestDistance(links, source, target)
//        val backward = findShortestDistance(links, target, source)
//        //println("computed " + source + "to" + target)
//        if (forward != -1) {
//          // there is a path going from source to target
//          validPairs += ((source, target, forward))
//        } else if (backward != -1) {
//          // there is a path going from target to source
//          validPairs += ((target, source, backward))
//        } else {
//          // target and source are parallel
//          validPairs += ((source, target, 0))
//        }
//      }
//
//    validPairs.toList foreach { x =>
//      val source = x._1
//      val target = x._2
//      val dg = x._3
//      
//      val forward = hashTable.get((source, target)).getOrElse((0, 0))
//      val backward = hashTable.get((target, source)).getOrElse((0, 0))
//      
//      //println(forward + " " + backward )
//      
//      val numerator = forward._1 - backward._1
//      val denominator = forward._2 + backward._2
//      //println(numerator + " / " + denominator)
//      val dn = 
//        if (denominator == 0) 0.0 // avoiding divide by zero
//        else numerator / denominator.toDouble
//        
//      val difference = dn - dg // dn is expected and dg is actual
//      sum += difference * difference
//      total += 1
//      hash.put(new Link(source, target), (dn, dg))
//    }
//
//    val error = sum
//    val avg = error / total
//    println("sum squared = " + error + ", avg = " + avg + " number of pairs = " + total)
//    (error, avg)
//  }
//
//
//}