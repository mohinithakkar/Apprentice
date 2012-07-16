package graph

import data._
import parse._
import utils._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object Causality {

  val MAX_OR = 2
  

  def findCausal(storyList: List[Story], graph: Graph): Graph = {
    val usedClusters = graph.usedClusters()
    val total = usedClusters.length

    // combination contains (clusterA, clusterB) where clusterA must have a path to clusterB
    //    val combinations =
    //      for (i <- 0 until total) yield {
    //        val predecessors =
    //          for (j <- 0 until total if i != j && graph.shortestDistance(usedClusters(j), usedClusters(i)) != -1) yield usedClusters(j)
    //
    //        (usedClusters(i), predecessors)
    //      }

    val combinations = graph.links.groupBy { l => l.target }.map {
      case (cluster, preLinks) =>
        val predecessors = preLinks.map { _.source }.distinct
        (cluster, predecessors)
    }
    
    println(combinations.map{
      case (cluster, predecessors) => cluster.name + " : " + predecessors.map(_.name).mkString("; ")
    }.mkString("\n"))

    //    val pr =
    //    for (c <- combinations) yield {
    //      val s = c._2.map(_ name)
    //      (c._1.name, s)
    //    }
    //    
    //    println(pr)

    var result = new HashMap[Cluster, List[List[Cluster]]]

    for (pair <- combinations) {
      var c = pair._1
      var set = pair._2.toList

      for (i <- 1 to MAX_OR) {

        val candidates = Combinatorics.combinations(i, set)

        // why would this happen? should not happen I think
        //        if (GraphAlgo.findShortestDistance(links, a, b) == -1) {
        //          var temp = a
        //          a = b
        //          b = temp
        //        }

        candidates foreach { can =>
          // (A or B) -> C
          val AorBList = can.toList
          // computing P(A or B | C)
          // all stories containing C
          val storyC = ANDStories(List(c), storyList)
          // all stories containing A or B
          val storyAB = ORStories(AorBList, storyList)
          val prob1 = storyAB.intersect(storyC).length / storyC.length.toDouble

          // computing P(not C | not (A or B))
          val storyNotC = ANDNotStories(List(c), storyList)
          // computing P(not (A or B))
          val storyNotAB = ORNotStories(AorBList, storyList)
          val prob2 = storyNotAB.intersect(storyNotC).length / storyNotAB.length.toDouble
          //println(" " + pnA + " " + pnAB)
          if (prob2 > 0.85 && prob1 > 0.85 && storyNotAB.length > 3 && storyC.length > 3) {
            val betterExplanations = result.getOrElse(c, Nil)
            if (!betterExplanations.exists(x => x.filterNot(AorBList.contains(_)) isEmpty)) // no better explanation exists
            {
              //if (c.name == "use bathroom")
              //println("possible causal: " + AorBList.map(_.name).mkString(" || ") + " ->, " + c.name + ", " + prob1 + ", " + prob2)
              result += (c -> (AorBList :: betterExplanations))
            }
            //            else {
            //              if (c.name == "use bathroom") {
            //                println("not causal: " + AorBList.map(_.name).mkString(" || ") + " ->, " + c.name + ", " + prob1 + ", " + prob2)
            //                println("because " + betterExplanations.filter(x => x.filterNot(AorBList.contains(_)) isEmpty).map(_.map(_.name)))
            //              }
            //            }
          }
        }
      }
    }

    var causalLinks = result.flatMap {
      case (follower, causerList) =>
        causerList.flatMap { _.map { x => new Link(x, follower, "C") } }
    }.toList

    causalLinks = causalLinks.distinct

    for (i <- 0 until causalLinks.length; j <- i + 1 until causalLinks.length if causalLinks(i) == causalLinks(j))
      println(i + " " + j + " " + causalLinks(i) + "   " + causalLinks(j))

    //println(causalLinks.map(x => x.toString + " " + x.kind).mkString("\n"))

    new Graph(graph.nodes, causalLinks ::: graph.links)
  }

  /**
   * given a list of clusters (A, B, ...), find the number of stories that contains sentences in A or B or ...
   *
   */
  def ORStories(clusters: List[Cluster], storyList: List[Story]): List[Story] =
    {
      storyList.filter {
        story =>
          clusters exists {
            cluster => story.members exists { x => cluster.members.contains(x) }
          }
      }
    }

  /**
   * given a list of clusters (A, B, ...), find the number of stories that contains no sentences in A and B and ...
   *
   */
  def ORNotStories(clusters: List[Cluster], storyList: List[Story]): List[Story] =
    {
      storyList.filterNot {
        story =>
          clusters exists {
            cluster => story.members exists { x => cluster.members contains (x) }
          }
      }
    }

  /**
   * given a list of clusters (A, B, ...), find the number of stories that contains sentences in A and sentences in B and ...
   *
   */
  def ANDStories(clusters: List[Cluster], storyList: List[Story]): List[Story] =
    {
      storyList.filter { story =>
        clusters.forall {
          cluster => story.members.exists { x => cluster.members.contains(x) }
        }
      }
    }

  /**
   * given a list of clusters (A, B, ...), find the number of stories that contains no sentences in A or B or ...
   *
   */
  def ANDNotStories(clusters: List[Cluster], storyList: List[Story]): List[Story] =
    {
      storyList.filterNot { story =>
        clusters.exists {
          cluster => story.members.exists { x => cluster.members.contains(x) }
        }
      }
    }
}