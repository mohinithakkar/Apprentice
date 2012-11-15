package edu.gatech.eilab.scheherazade

import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer;
import analysis.Cooccurence;
import utils._
import java.io._
import graph._
import data._
import graph.metric._

package graph {
  class GraphGenerator(stories: List[Story], clusters: List[Cluster]) {

    var PROBABILITY_THRESHOLD: Double = 0
    var MUTUAL_INFO_THRESHOLD: Double = 0

    val storyList: List[Story] = stories
    val clusterList: List[Cluster] = clusters

    val errorChecker = new ErrorChecker()

    def checkMissingSentences(storyList: List[Story]): Boolean =
      {
        storyList foreach {
          story =>
            val missingSentences = story.members filter
              {
                sentence => sentence.cluster == null
              }
            if (!missingSentences.isEmpty) {
              println("missing sentences:\n" + missingSentences.mkString("\n"))
              return true
            }
        }
        false
      }

    def thresholdFilter = ((r: ObservedLink) => (r.confidence > PROBABILITY_THRESHOLD))

    /**
     * Generates the graph according to the property
     * returns everything in a hashmap:
     * GraphName:String -> (graph:Graph, graphError:Double)
     */
    def generate(property: SingleProperty): HashMap[String, (Graph, Double)] = {

      PROBABILITY_THRESHOLD = property.doubleParam("probThresholds")
      MUTUAL_INFO_THRESHOLD = property.doubleParam("miThresholds")

      println("generating plot graph using the following parameters: \n" +
        property.toString)

      val hashmap = new HashMap[String, (Graph, Double)]()
      var errorBefore = -1.0
      var errorAfter = -1.0

      val allRelations: List[ObservedLink] = computeRelations(storyList, clusterList).filter(_.totalObservations > 0)

      // create the graph that contains every link
      val links = allRelations filter thresholdFilter
      val totalGraph = new Graph(clusterList, links)

      var compactGraph = totalGraph.compact

      if (compactGraph.containsLoop()) {
        hashmap += ("withLoops" -> (compactGraph, 0))
        // find the loops and break them
        try {
          compactGraph = breakLoops(compactGraph, links)
        } catch {
          case ge: GraphException =>
            println(ge.msg)
            hashmap += ("original" -> (compactGraph, 0))
            return hashmap
        }
      }

      //println(compactGraph.links.mkString("\n"))
      var (sum, avg) = errorChecker.checkErrors(storyList, compactGraph)
      println("before improvement, avg err = " + avg)

      hashmap += (("original", (compactGraph, avg)))

      // improve the graph
      val improvedGraph = updateBadPaths2(errorChecker.getBadPaths, compactGraph, allRelations)
      avg = errorChecker.checkErrors(storyList, improvedGraph)._2
      errorAfter = avg
      hashmap += (("improved", (improvedGraph, avg)))

      // TODO compute mutual exclusions below
      val mes = findMtlExcl(storyList, clusterList, MUTUAL_INFO_THRESHOLD)
      val meGraph = new Graph(improvedGraph.nodes, improvedGraph.links, mes);

      hashmap += (("mutualExcl", (meGraph, avg)))

      hashmap

    }

    def breakLoops(graph: Graph, links:List[ObservedLink]): Graph =
      {
        val loops = graph.simpleLoops
        var allLinks = links
        for (loop <- loops) {

          // find all relations in the loop
          val slidingWindows = List(loop.last, loop.head) :: loop.sliding(2).toList
          
          var loopLinks = slidingWindows.map {
            pair =>
              val s = pair.head
              val t = pair.last
              allLinks.find(l => l.source == s && l.target == t).get.asInstanceOf[ObservedLink]
          }

          // find the link with the minimum confidence
          var minLink: ObservedLink = null
          var min = 1.0
          for (link <- loopLinks) {
            if (link.confidence < min) {
              min = link.confidence
              minLink = link
            }
          }

          // remove that link
          allLinks = allLinks filterNot (_ == minLink)
        }

        val answer = new Graph(graph.nodes, allLinks).compact()
        if (answer.containsLoop)
          throw new GraphException("Encountered complex loop structure that cannot be removed by this simple procedure")
        else return answer
      }

    def findMtlExcl(stories: List[Story], clusters: List[Cluster], threshold: Double) = {

      var melinks = ListBuffer[MutualExcl]()
      val size = clusters.size
      for (i <- 0 until size; j <- i + 1 until size) {
        val c1 = clusters(i)
        val c2 = clusters(j)

        val mi = Cooccurence.mutualInfo(c1, c2, stories)
        if (mi._1 + mi._2 > threshold && mi._2 > 0)
          melinks += new MutualExcl(c1, c2)
      }
      melinks.toList
    }

    def updateBadPaths2(badPaths: List[(Link, (Double, Double))], graph: Graph,
      allRelations: List[ObservedLink]): Graph =
      {
        var relationsBelow = allRelations filter { rel =>
          !graph.ordered(rel.source, rel.target)
        } sortWith (_.confidence > _.confidence)

        var newLinks = graph.links
        var newGraph = graph
        var oldGraph = graph

        var oldErr = errorChecker.checkErrors(storyList, graph)._2
        var newErr = oldErr

        var accepted = 0
        var rejected = 0

        var success = true
        while (success) {
          val candidate = relationsBelow.head
          relationsBelow = relationsBelow.tail
          //if (newRelations.find(r => r.source == possible && r.target == target).isDefined) {

          println("Evaluating: " + candidate.source.name + " -> " + candidate.target.name)

          newLinks = new Link(candidate.source, candidate.target) :: oldGraph.links
          newGraph = new Graph(graph.nodes, newLinks).compact
          newErr = errorChecker.checkErrors(storyList, newGraph)._2

          if (newErr >= oldErr) {
            success = false
            // the total error has increased or stay constant. undo that update
            // this is a change from previous approach which only requires the error not to increase. 
            // This seems to curb the indeterminism
            //newRelations = oldRelations
            //newGraph = oldGraph
          } else {
            println("accepted " + candidate.source.name + " -> " + candidate.target.name)
            accepted += 1
            oldErr = newErr // total error decreased. update succeeded.
            oldGraph = newGraph
          }
        }
        // case None =>
        //}
        //else println("rejected for non existence in original links")

        //if (!updateSuccess) return newRelations
        println("accepted modifications: " + accepted + " rejected modifications: " + rejected)
        oldGraph
      }

    def updateBadPaths3(badPaths: List[(Link, (Double, Double))], graph: Graph,
      allRelations: List[ObservedLink]): Graph =
      {
        var relationsBelow = allRelations filter { rel =>
          !graph.ordered(rel.source, rel.target)
        } sortWith (_.confidence > _.confidence)

        var newLinks = graph.links
        var newGraph = graph
        var oldGraph = graph

        var oldErr = errorChecker.checkErrors(storyList, graph)._2
        var newErr = oldErr

        var accepted = 0
        var rejected = 0

        var success = true
        while (relationsBelow != Nil) {
          val candidate = relationsBelow.head
          relationsBelow = relationsBelow.tail
          //if (newRelations.find(r => r.source == possible && r.target == target).isDefined) {

          println("Evaluating: " + candidate.source.name + " -> " + candidate.target.name)

          newLinks = new Link(candidate.source, candidate.target) :: oldGraph.links
          newGraph = new Graph(graph.nodes, newLinks).compact
          newErr = errorChecker.checkErrors(storyList, newGraph)._2

          if (newErr >= oldErr) {
            success = false
            // the total error has increased or stay constant. undo that update
            // this is a change from previous approach which only requires the error not to increase. 
            // This seems to curb the indeterminism
            //newRelations = oldRelations
            //newGraph = oldGraph
            rejected += 1
          } else {
            println("accepted " + candidate.source.name + " -> " + candidate.target.name)
            accepted += 1
            oldErr = newErr // total error decreased. update succeeded.
            oldGraph = newGraph
          }
        }
        // case None =>
        //}
        //else println("rejected for non existence in original links")

        //if (!updateSuccess) return newRelations
        println("accepted modifications: " + accepted + " rejected modifications: " + rejected)
        oldGraph
      }

    def updateBadPaths(badPaths: List[(Link, (Double, Double))], graph: Graph,
      allRelations: List[ObservedLink]): Graph =
      {

        var newRelations = allRelations
        var oldRelations = allRelations

        var newLinks = graph.links
        var newGraph = graph
        var oldGraph = graph

        var oldErr = errorChecker.checkErrors(storyList, graph)._2
        var newErr = oldErr

        var accepted = 0
        var rejected = 0

        // sort the pairs of events in decreasing order of graph error
        val sorted = badPaths.sortWith {
          (x, y) => math.abs(x._2._1) - x._2._2 > math.abs(y._2._1) - y._2._2
        }

        sorted foreach {
          path =>
            val link = path._1

            var expected = math.abs(path._2._1)
            val actual = path._2._2
            val deviation = expected - actual

            var source = link.source
            var target = link.target

            if (path._2._1 < 0) {
              source = link.target
              target = link.source
            }

            println("processing bad link: " + source.name + " -> " + target.name + " expected = " + expected + " actual = " + actual + " deviation =" + deviation)

            var possibleSources = newGraph.takeSteps(source, math.round(expected - 1).toInt)

            possibleSources = possibleSources filter
              {
                possible =>
                  !oldGraph.ordered(target, possible)
                /* This prevents cycles. 
                 * If there is already a path from target to this possible source,
                 * we will create a cycle. 
                 * If there is already a path from this possible source to target, this will not help
                 */
              }

            // find possible new sources for our target event
            // That is, what new sources would create the desired separation?
            var possible = for (posSource <- possibleSources) yield {
              val forward = oldRelations.find(r => r.source == posSource && r.target == target)
              val backward = oldRelations.find(r => r.target == posSource && r.source == target)
              if (forward.isDefined) (posSource, forward.get.confidence)
              else if (backward.isDefined) (posSource, 1 - backward.get.confidence)
              else (posSource, 0.5)
            }

            // sorts the possible new sources by the confidence of the link (new_possible_source, target)
            possible = possible.sortWith((x, y) => x._2 > y._2)

            possible foreach {
              case (posSource, conf) =>
                if (conf > 0.3) {
                  //if (newRelations.find(r => r.source == possible && r.target == target).isDefined) {

                  println("Evaluating: " + posSource.name + " -> " + target.name)
                  // add a number of positive relations                     
                  val updated = new ObservedLink(posSource, target, 1, 1)
                  oldRelations = newRelations

                  //                  newRelations.find(r => r.source == possible && r.target == target) match {
                  //                    case None => newRelations = updated :: newRelations 
                  //                    case _ => 
                  //                  }

                  newLinks = new Link(posSource, target) :: oldGraph.links.filterNot(l => l.source == source && l.target == target)
                  newGraph = new Graph(graph.nodes, newLinks).compact
                  newErr = errorChecker.checkErrors(storyList, newGraph)._2

                  if (newErr >= oldErr) {
                    println("rolled back")
                    rejected += 1
                    // the total error has increased or stay constant. undo that update
                    // this is a change from previous approach which only requires the error not to increase. 
                    // This seems to curb the indeterminism
                    newRelations = oldRelations
                    newGraph = oldGraph
                  } else {
                    println("accepted " + posSource.name + " -> " + target.name)
                    accepted += 1
                    oldErr = newErr // total error decreased. update succeeded.
                    oldGraph = newGraph
                    oldRelations = newRelations
                  }
                }
            }

          //if (!updateSuccess) return newRelations
        }
        println("accepted modifications: " + accepted + " rejected modifications: " + rejected)
        oldGraph
      }

    def computeRelations(storyList: List[Story], clusterList: List[Cluster]): List[ObservedLink] =
      {
        var relations = List[ObservedLink]()
        val linkTable = new HashMap[(Cluster, Cluster), Int]

        def increment(source: Cluster, target: Cluster) {
          if (linkTable.contains((source, target))) {
            // increment the count
            var count = linkTable((source, target))
            count += 1
            linkTable((source, target)) = count
          } else
            linkTable += { (source, target) -> 1 }
        }

        storyList foreach {
          story =>
            for (i <- 0 to story.members.length - 2) {
              for (j <- i + 1 to story.members.length - 1) {
                val source = story.members(i).cluster
                val target = story.members(j).cluster // this line for normal computation
                //val target = story.members(i+1).cluster // this line for adjacent graph

                //if (source == null || target == null) println(story + ", " + i + ", " + j + " have no clusters")
                // $source and $target can be null. in which case they will be ignored in counting later 
                increment(source, target)
              }
            }
        }

        var linkList = List[(Cluster, Cluster, Int)]()
        //val clusterArray = clusterList.toArray // efficiency increase? should refactor to use lists

        var sourceList = clusterList
        var source: Cluster = null
        var target: Cluster = null

        // non-repeated counting
        while (sourceList != Nil) {
          source = sourceList.head
          sourceList = sourceList.tail
          var targetList = sourceList
          while (targetList != Nil) {
            target = targetList.head
            targetList = targetList.tail

            val forward = linkTable.getOrElse((source, target), 0)
            val backward = linkTable.getOrElse((target, source), 0)
            //println("forward: " + forward + " backward: " + backward)
            val forwardRelation = ObservedLink(source, target, forward, forward + backward)
            val backwardRelation = ObservedLink(target, source, backward, forward + backward)
            relations = forwardRelation :: backwardRelation :: relations
          }
        }

        relations
      }

    def printDistance(distList: List[(Cluster, Cluster, Double)]) {
      println(distList.map {
        entry =>
          entry._1.name.replace(" ", "_") + " -> " + entry._2.name.replace(" ", "_") + ": " + entry._3
      }.mkString("/n"))
    }

    //    def countLinks(storyList: List[Story], clusterList: List[Cluster], threshold: Int): List[ClusterLink] = {
    //      // now count links
    //      val linkTable = new HashMap[(Cluster, Cluster), ClusterLink]
    //
    //      def increment(source: Cluster, target: Cluster) {
    //        if (linkTable.contains((source, target))) {
    //          linkTable.get((source, target)).get.increment()
    //        } else
    //          linkTable += { (source, target) -> new ClusterLink(source, target, 1) }
    //      }
    //
    //      storyList foreach {
    //        story =>
    //          for (i <- 0 to story.members.length - 1) {
    //            for (j <- i + 1 to story.members.length - 1) {
    //              val source = story.members(i).cluster
    //              val target = story.members(j).cluster
    //              //if (source == null || target == null) println(story + ", " + i + ", " + j + " have no clusters")
    //              increment(source, target)
    //            }
    //          }
    //      }
    //
    //      var linkList = List[ClusterLink]()
    //      val clusterArray = clusterList.toArray
    //
    //      //val differenceThreshold = 4
    //
    //      for (i <- 0 to clusterArray.length - 1) {
    //        for (j <- i + 1 to clusterArray.length - 1) {
    //          val source = clusterArray(i)
    //          val target = clusterArray(j)
    //          val forwardLink = linkTable.get((source, target))
    //          val forward = forwardLink.map { _.count }.getOrElse(0)
    //          val backwardLink = linkTable.get((target, source))
    //          val backward = backwardLink.map { _.count }.getOrElse(0)
    //          //println("forward: " + forward + " backward: " + backward)
    //          if (forward - backward >= threshold && forward > 0)
    //            linkList = forwardLink.get :: linkList
    //          else if (backward - forward >= threshold && backward > 0)
    //            linkList = backwardLink.get :: linkList
    //        }
    //      }
    //
    //      linkList.sorted
    //    }
    //
    //    def printLinks(linkList: List[ClusterLink]) {
    //      // this is where the full graph is output
    //      println(getLinksText(linkList))
    //    }
    //
    //    def getLinksText(linkList: List[ClusterLink]) = {
    //      // this is where the full graph is output
    //      linkList map { x => x.source.name.replace(" ", "_") + " -> " + x.target.name.replace(" ", "_") } mkString ("\r\n")
    //    }

    def getRelationsText(relationList: List[ObservedLink]) = {
      // this is where the full graph is output
      relationList map { x => x.source.name.replace(" ", "_") + " -> " + x.target.name.replace(" ", "_") } mkString ("\r\n")
    }

    def simplifyGraph(clusterList: List[Cluster], relationList: List[ObservedLink]): List[Link] = {
      // simplifying the graph
      val clusterNumber = clusterList zip (0 to clusterList.length - 1)

      val numbers = relationList map {
        relation =>
          val id1 = clusterNumber.filter { x => x._1 == relation.source }(0)._2
          val id2 = clusterNumber.filter { x => x._1 == relation.target }(0)._2
          (id1, id2)
      }

      val order = new Ordering(numbers.toSet[(Int, Int)])
      val small = order.necessary()
      //println(small)
      val finalLinks = small map {
        n =>
          val source = clusterNumber.filter { x => x._2 == n._1 }(0)._1
          val target = clusterNumber.filter { x => x._2 == n._2 }(0)._1
          new Link(source, target)
      }

      finalLinks.toList
    }

  }
}