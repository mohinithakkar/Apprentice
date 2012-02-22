package parse

import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap
import java.io._
import java.util.Properties

class GraphGenerator(stories: List[Story], clusters: List[Cluster], property: Properties) {

  val DEFAULT_PROB_THRESHOLD = 0.55
  val DEFAULT_CONF_THRESHOLD = 0.45

  val DEFAULT_ADDED_OBSERVATIONS = 1

  def getParameter[T](property: Properties, name: String, conversionFunc: String => T): Option[T] =
    {
      if (property.containsKey(name)) {
        val s = property.getProperty(name)
        try {
          Some(conversionFunc(s))
        } catch {
          case e: Exception =>
            println("illegal parameter value for " + name + " " + s)
            e.printStackTrace()
            None
        }
      } else
        None
    }

  val PROBABILITY_THRESHOLD: Double = getParameter(property, "probThresholds", x => x.toDouble).getOrElse(DEFAULT_PROB_THRESHOLD)

  val CONFIDENCE_THRESHOLD = getParameter(property, "confThresholds", x => x.toDouble).getOrElse(DEFAULT_CONF_THRESHOLD)

  val ADDED_OBSERVATIONS = getParameter(property, "addedObservations", x => x.toInt).getOrElse(DEFAULT_ADDED_OBSERVATIONS)

  val OUTPUT_FILE = getParameter(property, "outputFile", x => x).getOrElse("output")

  val storyList: List[Story] = stories
  val clusterList: List[Cluster] = clusters

  val errorChecker = new ErrorChecker2()

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

  /* returns the error before and after adjustment
   * 
   */
  def generate(): (Double, Double) = {

    println("generating plot graph using the following parameters: \n" +
      "probability threshold = " + PROBABILITY_THRESHOLD + "\n" +
      "confidence threshold = " + CONFIDENCE_THRESHOLD + "\n" +
      "added observations = " + ADDED_OBSERVATIONS + "\n")

    var errorBefore = -1.0
    var errorAfter = -1.0

    var statsList = List[(Int, Double, Double)]()

    val allRelations: List[Relation] = computeRelations(storyList, clusterList).filter(_.totalObservations > 0)
    val text = allRelations.sorted.mkString("\n")

    //println(text)

    val reducedLinks = drawDiagram(clusterList, allRelations, OUTPUT_FILE)

    val checker = errorChecker.getNewInstance()

    var (sum, avg) = checker.checkErrors(storyList, clusterList, reducedLinks)
    println("before improvement, avg err = " + avg)
    errorBefore = avg
    //println("GOOD PATHS: \n\n" + checker.getGoodPaths.mkString("\n"))
    //println("\n\n BAD PATHS: \n\n" + checker.getBadPaths.mkString("\n"))
    //statsList = (i, sum, avg) :: statsList

    //println("threshold = " + i + ", sum = " + sum + ", avg = " + avg)
    //      var relations = valid.filter { r =>
    //        reducedLinks.exists { link => link.source == r.source && link.target == r.target }
    //      }

    var relations = updateBadPaths(checker.getBadPaths, reducedLinks, allRelations, new ErrorChecker2().findShortestDistance)

    val reduced2 = drawDiagram(clusterList, relations, OUTPUT_FILE + "-adjusted")
    //println("hello " + relations.mkString("\n"))
    //      reduced2.foreach{
    //        r =>
    //          r.source + r.target + r.
    //      }

    avg = errorChecker.getNewInstance().checkErrors(storyList, clusterList, reduced2)._2
    println("after improvement, avg err = " + avg)
    errorAfter = avg
    //println(statsList.map(_.toString).map(x => x.substring(1, x.length - 2)).mkString("\n"))
    (errorBefore, errorAfter)

  }

def updateBadPaths(badPaths: List[(Link, (Double, Double))], links: List[Link],
    allRelations: List[Relation], shortestDistance: (List[Link], Cluster, Cluster) => Int): List[Relation] =
    {
      var newRelations = allRelations
      var oldRelations = allRelations

      var newLinks = links
      //var oldLinks = newLinks

      var oldErr = errorChecker.getNewInstance().checkErrors(storyList, clusterList, newLinks)._2
      var newErr = oldErr

      badPaths.sort {
        (x, y) => (x._2._1 - x._2._2) > (y._2._1 - y._2._2)
      } foreach {
        path =>
          val link = path._1
          val source = link.source
          val expected = path._2._1
          val deviation = path._2._1 - path._2._2
          val target = link.target
          //println("processing bad link " + source.name + " " + target.name + " expected = " + expected + " " + (expected-1).toInt + " deviation =" + deviation)
          var possibleSources = findPathEnds(source, (expected - 1).toInt, newLinks)

          //var updateSuccess = false
          
          possibleSources = possibleSources filter
            {
              possible =>
                
                val dist = shortestDistance(newLinks, target, possible)                
                dist == -1
                
                /* This prevents cycles. If there is already a path fro target to this posible source,
                 * and we strength the link from this possible source to target, 
                 * we could create a cycle.
                 */
                
            }
          
          var updateSuccess = (possibleSources.length == 0) 
          // if there are no possible sources, we would not give up on the next
          
          possibleSources foreach {
              possible =>                                
                newRelations.find(r => r.source == possible && r.target == target) match {
                  case Some(rel: Relation) =>
                  	//println ("possible source: " + possible.name)
                    val updated = rel.addEvidence(ADDED_OBSERVATIONS, 0)

                    oldRelations = newRelations
                    newRelations = updated :: (newRelations - rel)

                    // adding the new relation to the set of links if it already surpasses the threshold
                    newLinks = simplifyGraph(clusterList, newRelations.filter { r =>
                      r.confidence > CONFIDENCE_THRESHOLD && r.prob > PROBABILITY_THRESHOLD
                    })
                    newErr = errorChecker.getNewInstance().checkErrors(storyList, clusterList, newLinks)._2
                    //newErr = sum / total
                    //println("new error = " + newErr)
                    if (newErr > oldErr) {
                      newRelations = oldRelations // the total error has increased. undo that update
                    } else {
                      oldErr = newErr // total error decreased. update succeeded.
                      updateSuccess = true
                    }
                  case None =>
                }
            }
            
            //if (!updateSuccess) return newRelations
      }

      newRelations
    }

  def drawDiagram(clusterList: List[Cluster], allRelations: List[Relation], filename: String) = {

    val valid = allRelations.filter {
      relation => (relation.confidence > CONFIDENCE_THRESHOLD && relation.prob > PROBABILITY_THRESHOLD) // ||  (relation.confidence > 0.3 && relation.prob > 0.9)
    }

    val invalid = allRelations.filter {
      relation => relation.confidence <= 0.4 && relation.confidence > 0.3 && relation.prob > 0.4
    }

    val probWriter = new PrintWriter(new BufferedOutputStream(new FileOutputStream("probablities.txt")))
    valid.foreach { r =>
      probWriter.println(r.source.name + " -> " + r.target.name + ", " + r.prob + ", " + r.confidence)
    }

    probWriter.close()

    //println(invalid.sorted.mkString("\n"))

    val reducedLinks = simplifyGraph(clusterList, valid)
//
//    val fullWriter = new PrintWriter(new BufferedOutputStream(new FileOutputStream(filename + ".txt")))
//    println("writing to file: " + filename + ".png")
//    fullWriter.println("digraph G {")
//    fullWriter.println(getRelationsText(valid))
//    fullWriter.println("}")
//    fullWriter.close()
//
//    Runtime.getRuntime().exec("dot -Tpng -o" + filename + ".png " + filename + ".txt")
//    new File(filename + ".txt").deleteOnExit()

    val reducedWriter = new PrintWriter(new BufferedOutputStream(new FileOutputStream(filename + "-reduced.txt")))
    reducedWriter.println("digraph G {")
    reducedWriter.println(reducedLinks.mkString("\n"))
    reducedWriter.println("}")
    reducedWriter.close()

    Runtime.getRuntime().exec("dot -Tpng -o" + filename + "-reduced.png " + filename + "-reduced.txt")
    new File(filename + "-reduced.txt").deleteOnExit()

    reducedLinks
  }

  def findPathEnds(source: Cluster, steps: Int, links: List[Link]): List[Cluster] =
    {
      var ends = List[Cluster]()

      val distance = scala.collection.mutable.Queue[(Cluster, Int)]()
      var remaining = links
      distance += ((source, 0))

      while (distance != Nil) {
        val elem = distance.dequeue()
        val head = elem._1
        val dist = elem._2
        if (dist == steps) ends = head :: ends // find one end. add it to the list
        else {
          links.filter(link => link.source == head).foreach {
            link => distance.enqueue((link.target, dist + 1))
          }
        }
      }

      ends
    }

  def computeRelations(storyList: List[Story], clusterList: List[Cluster]): List[Relation] =
    {
      var relations = List[Relation]()
      val linkTable = new HashMap[(Cluster, Cluster), ClusterLink]

      def increment(source: Cluster, target: Cluster) {
        if (linkTable.contains((source, target))) {
          linkTable.get((source, target)).get.increment()
        } else
          linkTable += { (source, target) -> new ClusterLink(source, target, 1) }
      }

      storyList foreach {
        story =>
          for (i <- 0 to story.members.length - 1) {
            for (j <- i + 1 to story.members.length - 1) {
              val source = story.members(i).cluster
              val target = story.members(j).cluster
              //if (source == null || target == null) println(story + ", " + i + ", " + j + " have no clusters")
              increment(source, target)
            }
          }
      }

      var linkList = List[ClusterLink]()
      val clusterArray = clusterList.toArray

      //val differenceThreshold = 4

      for (i <- 0 to clusterArray.length - 1) {
        for (j <- i + 1 to clusterArray.length - 1) {
          val source = clusterArray(i)
          val target = clusterArray(j)
          val forwardLink = linkTable.get((source, target))
          val forward = forwardLink.map { _.count }.getOrElse(0)
          val backwardLink = linkTable.get((target, source))
          val backward = backwardLink.map { _.count }.getOrElse(0)
          //println("forward: " + forward + " backward: " + backward)
          val forwardRelation = Relation(source, target, forward, forward + backward)
          val backwardRelation = Relation(target, source, backward, forward + backward)
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

  def countLinks(storyList: List[Story], clusterList: List[Cluster], threshold: Int): List[ClusterLink] = {
    // now count links
    val linkTable = new HashMap[(Cluster, Cluster), ClusterLink]

    def increment(source: Cluster, target: Cluster) {
      if (linkTable.contains((source, target))) {
        linkTable.get((source, target)).get.increment()
      } else
        linkTable += { (source, target) -> new ClusterLink(source, target, 1) }
    }

    storyList foreach {
      story =>
        for (i <- 0 to story.members.length - 1) {
          for (j <- i + 1 to story.members.length - 1) {
            val source = story.members(i).cluster
            val target = story.members(j).cluster
            //if (source == null || target == null) println(story + ", " + i + ", " + j + " have no clusters")
            increment(source, target)
          }
        }
    }

    var linkList = List[ClusterLink]()
    val clusterArray = clusterList.toArray

    //val differenceThreshold = 4

    for (i <- 0 to clusterArray.length - 1) {
      for (j <- i + 1 to clusterArray.length - 1) {
        val source = clusterArray(i)
        val target = clusterArray(j)
        val forwardLink = linkTable.get((source, target))
        val forward = forwardLink.map { _.count }.getOrElse(0)
        val backwardLink = linkTable.get((target, source))
        val backward = backwardLink.map { _.count }.getOrElse(0)
        //println("forward: " + forward + " backward: " + backward)
        if (forward - backward >= threshold && forward > 0)
          linkList = forwardLink.get :: linkList
        else if (backward - forward >= threshold && backward > 0)
          linkList = backwardLink.get :: linkList
      }
    }

    linkList.sorted
  }

  def printLinks(linkList: List[ClusterLink]) {
    // this is where the full graph is output
    println(getLinksText(linkList))
  }

  def getLinksText(linkList: List[ClusterLink]) = {
    // this is where the full graph is output
    linkList map { x => x.source.name.replace(" ", "_") + " -> " + x.target.name.replace(" ", "_") } mkString ("\r\n")
  }

  def getRelationsText(relationList: List[Relation]) = {
    // this is where the full graph is output
    relationList map { x => x.source.name.replace(" ", "_") + " -> " + x.target.name.replace(" ", "_") } mkString ("\r\n")
  }

  def simplifyGraph(clusterList: List[Cluster], relationList: List[Relation]): List[Link] = {
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