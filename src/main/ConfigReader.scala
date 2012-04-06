package main

import parse._
import data._
import xml._
import javanlp._
import graph._
import java.io._
import java.util.Properties
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import edu.stanford.nlp.trees.semgraph.SemanticGraph
import edu.stanford.nlp.ling.IndexedWord;

class ConfigReader(val configFile: String) {

  val properties = new Properties()
  val in = new FileInputStream(configFile)
  properties.load(in)
  in.close()
  println("Parameters supplied: " + properties.getProperty("parameters"))
  val paraNames = properties.getProperty("parameters").split(",")

  def allParameters(): Array[Properties] =
    {
      var params: Array[Properties] = new Array[Properties](0)
      for (n <- paraNames) {
        val name = n.trim
        // for each parameter listed, we get a list of values
        val values = properties.getProperty(name).split(",")
        if (params.isEmpty) {
          params = values.map { v =>
            val p = new Properties()
            p.setProperty(name, v)
            p
          }
        } else {
          // for each existing Properties object, we append this parameter
          params = params flatMap { param =>
            values.map { v =>
              val p = param.clone().asInstanceOf[Properties]
              p.setProperty(name, v)
              p
            }
          }
        }
      }

      params
    }

  def initData(): (List[Story], List[Cluster]) =
    {
      val storyFile = properties.getProperty("storyFile")
      val clusterFile = properties.getProperty("clusterFile")

      //println("using story file: " + storyFile)
      var storyList: List[Story] = SimpleParser.parseStories("./data/movie/movieSimpleStories.txt")
      //GoldParser.parseStories(storyFile)

      storyList.foreach(_.addStoryLocation())

      println("using cluster file: " + clusterFile)
      val clusterList: List[Cluster] = initClusters(storyList, clusterFile)

      // This filters unused sentences in the stories
      //storyList = filterUnused(storyList, clusterList)
      (storyList, clusterList)
    }

  def initDataFilterUnused(): (List[Story], List[Cluster]) =
    {
      val storyFile = properties.getProperty("storyFile")
      val clusterFile = properties.getProperty("clusterFile")

      //println("using story file: " + storyFile)
      var storyList: List[Story] = SimpleParser.parseStories("./data/movie/movieSimpleStories.txt")
      //GoldParser.parseStories(storyFile)

      storyList.foreach(_.addStoryLocation())

      println("using cluster file: " + clusterFile)
      val clusterList: List[Cluster] = initClusters(storyList, clusterFile)

      // This filters unused sentences in the stories
      storyList = filterUnused(storyList, clusterList)
      (storyList, clusterList)
    }

  def filterUnused(storyList: List[Story], clusterList: List[Cluster]): List[Story] =
    {
      val used = clusterList.flatMap { _.members }
      storyList map { story =>
        val newMembers = story.members.filter { s => used.contains(s) }
        val str = story.members.filterNot { s => used.contains(s) }.map { _.toShortString() }.mkString("\n")
        println(str)
        new Story(newMembers)
      }
    }
  /**
   * initializing the clusters. assigning the sentences to the right story and cluster, so we
   *  do not create duplicate sentence objects.
   *
   */
  def initClusters(storyList: List[Story], clusterFile: String) =
    {
      val hashmap = new HashMap[Int, Sentence]
      storyList foreach {
        story =>
          story.members foreach
            {
              sentence =>
                if (hashmap.contains(sentence.id)) throw new ParsingException("sentence repeated" + sentence.id)
                hashmap += ((sentence.id, sentence))
            }
      }

      GoldParser.parseClusters(clusterFile) map {
        c =>
          val newMembers = c.members map
            {
              sentence =>
                // make sure we get the same sentence 
                hashmap.get(sentence.id).get
            }
          val newC = new Cluster(c.name, newMembers)
          newC.members foreach { s =>
            s.cluster = newC
          }
          newC
      }
    }

  //  def parseSentence(storyList: List[Story]): List[Story] =
  //    {
  //      //    val sentences
  //      val text = storyList.map {
  //        story =>
  //          story.members.map {
  //            sentence: Sentence => sentence.tokens.map(_.word).mkString(" ")
  //          }.mkString("\n")
  //      }.mkString("\n")
  //      //println("text = " + text)
  //      NLPWrapper.init()
  //      NLPWrapper.getParsed(text)
  //
  //      var allSents = storyList flatMap { _.members } toArray
  //      var sentBuffer = new ListBuffer[Sentence]()
  //      var i = 0
  //
  //      while (NLPWrapper.hasNextSentence) {
  //        NLPWrapper.processNextSentence()
  //        var tokensArray = NLPWrapper.getTokens()
  //        if (tokensArray.length > 1 || tokensArray(0)(0) != ".") { // filters out empty sentences with a single period
  //
  //          var tokenBuffer = new ListBuffer[Token]()
  //          for (i <- 0 to tokensArray.length - 1) {
  //            val t = tokensArray(i)
  //            tokenBuffer += new Token(i, t(0), t(1), t(2), { if (t(3) == "O") "" else t(3) })
  //          }
  //
  //          val tokens = tokenBuffer.toArray
  //          println(i + ": " + tokens.mkString(" "))
  //          val tree = NLPWrapper.getParseTree()
  //          val graph = NLPWrapper.getSemanticGraph()
  //
  //          val relations = graphToRelations(graph, tokens)
  //          sentBuffer += Sentence(allSents(i).id, tokens, tree, relations)
  //          //        println("old sentence " + i + " " + allSents(i).tokens.map(_.word).mkString(" "))
  //          println(tokens.mkString(" "))
  //          println(relations.mkString("\n"))
  //          i += 1 // going through two lists simultaneously
  //        }
  //      }
  //
  //      var allNewSents = sentBuffer.toList
  //      var storyBuffer = new ListBuffer[Story]()
  //
  //      storyList.foreach { story =>
  //        val length = story.members.length
  //        storyBuffer += new Story(allNewSents.take(length).toArray)
  //        allNewSents = allNewSents.drop(length)
  //      }
  //
  //      storyBuffer.toList
  //    }

  def graphToRelations(graph: SemanticGraph, tokens: Array[Token]): List[Dependency] = {
    var relations = new ListBuffer[Dependency]()
    var queue = new Queue[(IndexedWord, Int)]()
    var used = List[IndexedWord]()
    val root = graph.getFirstRoot()

    queue += ((root, 0))

    while (!queue.isEmpty) {
      val (item, depth) = queue.dequeue()
      val it = graph.outgoingEdgeIterable(item).iterator

      while (it.hasNext) {
        val edge = it.next()
        val gov = edge.getGovernor()
        val dep = edge.getDependent()
        var relation = edge.getRelation()

        // makes sure each word is added to the queue only once
        if (!used.contains(dep)) {
          queue += ((dep, depth + 1))
          used = dep :: used
        }

        var specifics = relation.getSpecific()
        if (specifics == null) specifics = ""
        relations += new Dependency(tokens(gov.index() - 1), tokens(dep.index() - 1),
          relation.getShortName(), specifics, depth)

      }
    }

    relations.toList
  }

  def printParameterNames(pw: PrintWriter) {
    paraNames foreach { name =>
      pw.print(name.trim + ", ")
    }
    pw.println()
  }

  def printParameterValues(para: Properties, pw: PrintWriter) {
    paraNames foreach { name =>
      val value = para.getProperty(name.trim)
      pw.print(value + ", ")
    }
  }
}

object ConfigReader {
  def main(args: Array[String]) {
    //    val string = scala.io.Source.fromFile("movieParsed.txt").mkString    
    //    val obj = XStream.fromXML(string).asInstanceOf[StorySet]
    //    println(obj.storyList.mkString("\n"))
    val reader = new ConfigReader("configMv3.txt")
    val (stories, clusters) = reader.initData()
    val parser = new StoryNLPParser(stories, "movieParsed.txt", true)
    // val zero = s.storyList(0)
    //    println(zero)
    //    println(zero.members.mkString("\n"))

    def sentFn: () => List[Sentence] = () => parser().storyList.flatMap(_.members)

    val simi = new DSDSimilarity(sentFn, "movieSemantic.txt")
    val matrix1 = simi()
    //    utils.Matrix.prettyPrint(matrix1)
    //    System.exit(0)
    val local = new SimpleLocation(sentFn, 0.9, "movieLocations.txt")

    var addition = new MatrixAddition(() => simi(), () => local(), 0.3, "movie1stSimilarity.txt")

    //val matrix = simi()
    var matrix = addition()
    //utils.Matrix.prettyPrint(matrix)
    //        println("sents = " + sentList.length)
    //        println("matrix length = " + matrix.length)

    //no-link constraints
    var count = 0
    for (story <- stories) {
      val storyLen = story.members.length
      for (i <- 0 until storyLen; j <- i + 1 until storyLen) {
        matrix(i + count)(j + count) = 0
        matrix(j + count)(i + count) = 0
      }
      count = storyLen
    }

    var max: Double = 0
    var distance = matrix.map { a =>
      a.map { value =>
        if (value != 0) {
          val v = 1 / value
          if (v > max) {
            max = v
            println("smallest " + value)
          }
          v
        } else Double.PositiveInfinity
      }
    }

    println("v = " + max)

    val clusterList = cluster.algo.OPTICS.cluster(distance, max, 4, stories.flatMap(_.members.toList))

    
    // the first 5 big clusters
    val big = clusterList.sortWith((c1, c2) => c1.members.length > c2.members.length).take(3)
    //println(big.mkString("\n"))
    val clusOrder = order(stories, big)

    //println(clusOrder.mkString("\n"))
    import scala.collection.mutable.HashMap
    var numbers = new HashMap[Cluster, Int]()
    for (i <- 0 until clusOrder.length) numbers += (clusOrder(i) -> (i + 1))

    println(numbers.mkString("\n"))

    for (story <- stories) {
      var interval = ListBuffer[Sentence]()
      var prev = 0
      for (sent <- story.members) {
        sent.location = 0
        val base = clusOrder.find(_.members.contains(sent))
        base match {
          case Some(c: Cluster) =>
            val cur = numbers(c)
            sent.location = numbers(c)
            assignLoc(interval.toList, prev, cur)
            prev = cur
            interval.clear()
          case None => interval += sent
        }
      }
    }
    
    val betterDist = new SimpleLocation(() => stories.flatMap(_.members), 1, "movieBetterLocations.txt")
    addition = new MatrixAddition(() => simi(), () => betterDist(), 0.2, "movie2ndSimilarity.txt")

    matrix = addition()

    // no-link constraints
    count = 0
    for (story <- stories) {
      val storyLen = story.members.length
      for (i <- 0 until storyLen; j <- i + 1 until storyLen) {
        matrix(i + count)(j + count) = 0
        matrix(j + count)(i + count) = 0
      }
      count = storyLen
    }

    max = 0
    distance = matrix.map { a =>
      a.map { value =>
        if (value != 0) {
          val v = 1 / value
          if (v > max) {
            max = v
            println("smallest " + value)
          }
          v
        } else Double.PositiveInfinity
      }
    }

    //cluster.algo.OPTICS.loose = true
    val fclusters = cluster.algo.OPTICS.cluster(distance, max, 4, stories.flatMap(_.members.toList))

    //fclusters.

  }

  def assignLoc(list: List[Sentence], prev: Int, cur: Int) {
    val step: Double = (cur - prev).toDouble / (list.length + 1)
    for (i <- 0 until list.length) {
      list(i).location = (i + 1) * step + prev
      println(list(i).toShortString() + " : " + list(i).location)
    }

    println("\n")
  }

  def order(stories: List[Story], clusters: List[Cluster]): List[Cluster] = {

    import scala.collection.mutable.HashMap
    var map = new HashMap[(Cluster, Cluster), (Int, Int)]()
    var numbers = new HashMap[Cluster, Int]()
    for (i <- 0 until clusters.length) numbers += (clusters(i) -> i)

    val storyList = stories.map { s =>
      val memb = s.members.filter(sent => clusters.exists(_.members.contains(sent)))
      new Story(memb)
    }.filter { _.members.length > 0 }

    for (story <- storyList) {
      val members = story.members
      for (i <- 0 until members.length; j <- i + 1 until members.length if members(i).cluster != members(j).cluster) {
        val cpair = (members(i).cluster, members(j).cluster)
        val rpair = (members(j).cluster, members(i).cluster)
        if (map.contains(cpair)) {
          val t = map(cpair)
          map += (cpair -> (t._1 + 1, t._2))
        } else if (map.contains(rpair)) {
          val t = map(rpair)
          map += (rpair -> (t._1, t._2 + 1))
        } else {
          map += (cpair -> (1, 0))
        }
      }
    }

    var ordering = List[(Int, Int)]()

    println(map.map(x => x._1._1.name + " -> " + x._1._2.name + " " + x._2).mkString("\n"))
    map.foreach {
      case (pair, count) =>
        if (count._1 > count._2) ordering = (numbers(pair._1), numbers(pair._2)) :: ordering
        else if (count._2 > count._1) ordering = (numbers(pair._2), numbers(pair._1)) :: ordering
    }

    val result = new Ordering(ordering.toSet).topsort().map(clusters(_))

    result
  }

  def xml() {
    val reader = new ConfigReader("configMv3.txt")
    val (stories, clusters) = reader.initData()
    val xml = data.XStream.toXML(stories)
    val filename = "movieParse.xml"
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(filename)))
    pw.println("<Root>")
    pw.println(xml)
    pw.println("</Root>")
    pw.close()
    println("finished printing to file: " + filename)

    pw.close()
    Thread.sleep(2000)
  }

  def tempGraph(stories: List[Story], clusters: List[Cluster]) = {
    val reader = new ConfigReader("configMvn.txt")
    val para = reader.allParameters()(0)
    val outputPath = new File(reader.properties.getProperty("storyFile")).getParent()
    para.put("outputFile", outputPath + "\\conft")
    Relation.init(para)

    val gen = new GraphGenerator(stories, clusters, para)
    val (prevErr, prevFreedom, afterErr, afterFreedom) = gen.generate()
  }

  def generateGraphs() {
    val reader = new ConfigReader("configMv3.txt")
    val (stories, clusters) = reader.initData()
    for (s <- stories) println(s)

    val parameters = reader.allParameters()
    val outputPath = new File(reader.properties.getProperty("storyFile")).getParent()
    var i = 1;
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(outputPath + "\\summary.csv")));

    reader.printParameterNames(pw)

    parameters foreach { para =>
      reader.printParameterValues(para, pw)
      para.put("outputFile", outputPath + "\\conf" + i)
      println(outputPath + "conf" + i)
      i += 1

      Relation.init(para)
      val gen = new GraphGenerator(stories, clusters, para)
      val (prevErr, prevFreedom, afterErr, afterFreedom) = gen.generate()
      pw.print(prevErr + ", " + afterErr + ", " + prevFreedom + ", " + afterFreedom + ", " + "\n")

    }
  }
}