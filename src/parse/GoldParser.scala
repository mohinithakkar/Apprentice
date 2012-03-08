package parse

import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap
import java.io._
import data._
object GoldParser extends JavaTokenParsers {
  protected def word: Parser[String] = """[-’\w\.]+""".r
  protected def pos: Parser[String] = """[A-Z\$\.]+""".r
  protected def token: Parser[Token] = word ~ "/" ~ pos ^^ {
    case word ~ "/" ~ pos => Token(word, pos)
  }

  protected def sentence: Parser[Sentence] = wholeNumber ~ rep(token) ^^
    {
      case number ~ list =>
        Sentence(number.toInt, list)
    }

  protected def cluster: Parser[Cluster] = """@\s""".r ~> """[-\w\s]+\n""".r ~ rep(sentence) <~ "###" ^^
    {
      case name ~ list =>
        //println("Parsed cluster " + name.trim)
        new Cluster(name.trim, list)
    }

  protected def story: Parser[Story] = rep(sentence) <~ "###" ^^
    {
      case list =>
        val array = list.toArray[Sentence]
        for (i <- 0 to array.length - 1) {
          if (i < array.length - 1)
            array(i).next = array(i + 1)
        }

        val story = new Story(array)
        //        /println(story)
        story
    }

  protected def stories: Parser[List[Story]] = rep(story)

  def parseStories(filename: String): List[Story] =
    {
      val storiesText = scala.io.Source.fromFile(filename).mkString
      try {
        parseAll(stories, storiesText).get
      } catch {
        case e: RuntimeException =>
          e.printStackTrace()
          println(e.getLocalizedMessage())
          throw new RuntimeException()
      }
    }

  def parseClusters(filename: String): List[Cluster] =
    {
      val clusterText = scala.io.Source.fromFile(filename).mkString
      parseAll(rep(cluster), clusterText).get
    }

  def main(args: Array[String]) {

    val storyList = parseStories("stories.txt")

    // insert sentences into a hashtable (id -> sentence)
    val hashmap = new HashMap[Int, Sentence]
    storyList foreach {
      story =>
        story.members foreach
          {
            sentence =>
              if (hashmap.contains(sentence.id)) throw new RuntimeException("weird!" + sentence.id)
              hashmap += ((sentence.id, sentence))
          }
    }

    // establish the clusters, and mark each sentence with their cluster
    var clusterList = parseClusters("goldStandard.txt")
    println(clusterList)

    clusterList = clusterList map {
      c =>
        val newMembers = c.members map
          {
            sentence =>
              //println(sentence.id)
              hashmap.get(sentence.id).get
          }
        val newC = new Cluster(c.name, newMembers)
        newC.members foreach { s =>
          s.cluster = newC
        }
        newC
    }

    println(clusterList)
    val first = clusterList(0).members(0)
    println(first)
    val second = first.next
    if (second != null) {
      println(second)
      println(second.cluster.name)
    }

    var distances = computeDistance(storyList, clusterList)

    var statsList = List[(Int, Double, Double)]()

    for (i <- 0 to 7) {

      val allLinks = countLinks(storyList, clusterList, i)
      val reducedLinks = simplifyGraph(clusterList, allLinks)

      val fullWriter = new PrintWriter(new BufferedOutputStream(new FileOutputStream("full-" + i + ".txt")))
      fullWriter.println("digraph G {")
      fullWriter.println(getLinksText(allLinks))
      fullWriter.println("}")
      fullWriter.close()

      Runtime.getRuntime().exec("dot -Tpng -ofull-" + i + ".png " + "full-" + i + ".txt")

      val reducedWriter = new PrintWriter(new BufferedOutputStream(new FileOutputStream("reduced-" + i + ".txt")))
      reducedWriter.println("digraph G {")
      reducedWriter.println(getLinksText(reducedLinks))
      reducedWriter.println("}")
      reducedWriter.close()

      Runtime.getRuntime().exec("dot -Tpng -oreduced-" + i + ".png " + "reduced-" + i + ".txt")
      //printLinks(reducedLinks)

      distances = distances.filter {
        entry =>
          allLinks.exists { link => link.source == entry._1 && link.target == entry._2 }
      } // filter out those links we ignored during bidirectional selection

      val (sum, avg) = compareDist(distances, reducedLinks)
      //statsList = (i, sum, avg):: statsList 

      //val (sum, avg) = (new ErrorChecker()).checkErrors(storyList, clusterList, reducedLinks.map{l => new Link(l.source, l.target)})

      //println("threshold = " + i + ", sum = " + sum + ", avg = " + avg)
    }

    println(statsList.map(_.toString).map(x => x.substring(1, x.length - 2)).mkString("\n"))
  }

  def compareDist(distances: List[(Cluster, Cluster, Double)], reducedLinks: List[ClusterLink]): (Double, Double) = {
    var sum: Double = 0
    distances.foreach {
      x =>
        val source = x._1
        val target = x._2
        val distance = x._3

        val realDistance = findDistance(reducedLinks, source, target)
        //println(source.name + " -> " + target.name + ", expected: " + distance + " real: " + realDistance)
        sum += scala.math.abs(realDistance - distance)
    }
    println("sum = " + sum + ", avg = " + sum / distances.length)
    (sum, sum / distances.length)
  }

  def findDistance(links: List[ClusterLink], source: Cluster, target: Cluster): Int =
    {
      val distance = scala.collection.mutable.Queue[(Cluster, Int)]()
      var remaining = links
      distance += ((source, 0))

      while (distance != Nil) {
        val elem = distance.dequeue()
        val head = elem._1
        val dist = elem._2
        if (head == target) return dist
        else {
          links.filter(link => link.source == head).foreach {
            link => distance.enqueue((link.target, dist + 1))
          }
        }
      }

      -1
    }

  def computeDistance(storyList: List[Story], clusterList: List[Cluster]): List[(Cluster, Cluster, Double)] =
    {
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

      distList.toList
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
            if (source == null || target == null) println(story + ", " + i + ", " + j + " have no clusters")
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
    println("threshold = " + threshold + "\n" + linkList.sorted.mkString("\n"))
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

  def simplifyGraph(clusterList: List[Cluster], linkList: List[ClusterLink]): List[ClusterLink] = {
    // simplifying the graph
    val clusterNumber = clusterList zip (0 to clusterList.length - 1)

    val numbers = linkList map {
      link =>
        val id1 = clusterNumber.filter { x => x._1 == link.source }(0)._2
        val id2 = clusterNumber.filter { x => x._1 == link.target }(0)._2
        (id1, id2)
    }

    val order = new Ordering(numbers.toSet[(Int, Int)])
    val small = order.necessary()
    println(small)
    val finalLinks = small map {
      n =>
        val source = clusterNumber.filter { x => x._2 == n._1 }(0)._1
        val target = clusterNumber.filter { x => x._2 == n._2 }(0)._1
        new ClusterLink(source, target, 1)
    }

    finalLinks.toList
  }

}

