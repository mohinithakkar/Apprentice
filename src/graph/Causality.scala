package graph

import data._
import parse._
import utils._

object Causality {

  val MAX_OR = 3

  def findCausal(storyList: List[Story], clusterList: List[Cluster], links: List[Link]) {
    val usedClusters = links.flatMap { l => List(l.source, l.target) }.distinct.toArray
    val total = usedClusters.length

    // combination contains (clusterA, clusterB) where clusterA must have a path to clusterB
    val combinations =
      for (i <- 0 until total) yield {
        val predecessors =
          for (j <- i + 1 until total if GraphAlgo.ordered(links, usedClusters(j), usedClusters(i))) yield usedClusters(j)

        (usedClusters(i), predecessors)
      }

    combinations map { pair =>
      var c = pair._1
      var set = pair._2

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
          val storyNotAB = ORNotStories(AorBList, storyList)
          val prob2 = storyNotAB.intersect(storyNotC).length / storyNotAB.length.toDouble
          //println(" " + pnA + " " + pnAB)
          if (prob2 > 0.8 && prob1 > 0.8)
            println("possible causal: " + AorBList.mkString(" || ") + " -> " + c.name + " " + prob1 + ", " + prob2)
        }
      }
    }
  }

  /** given a list of clusters (A, B, ...), find the number of stories that contains sentences in A or B or ...
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

  /** given a list of clusters (A, B, ...), find the number of stories that contains no sentences in A and B and ...
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

  /** given a list of clusters (A, B, ...), find the number of stories that contains sentences in A and sentences in B and ...
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

  /** given a list of clusters (A, B, ...), find the number of stories that contains no sentences in A or B or ...
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