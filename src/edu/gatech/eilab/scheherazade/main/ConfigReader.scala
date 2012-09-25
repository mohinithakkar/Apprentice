package edu.gatech.eilab.scheherazade

import parse._
import data._
import xml._
import javanlp._
import graph._
import utils.SuperProperties
import java.io._
import edu.gatech.eilab.scheherazade.cluster.algo.OPTICS
import edu.gatech.eilab.scheherazade.cluster.metric.ClusterMetric
import java.util.Properties
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import edu.stanford.nlp.trees.semgraph.SemanticGraph
import edu.stanford.nlp.ling.IndexedWord;

package main {
  class ConfigReader(val configFile: String) {

    val properties = new SuperProperties()
    val in = new FileInputStream(configFile)
    properties.load(in)
    in.close()
    println("Parameters supplied: " + properties.getProperty("parameters"))

//    def initOldData(): (List[Story], List[Cluster]) =
//      {
//        val storyFile = properties.getProperty("storyFile")
//        val clusterFile = properties.getProperty("clusterFile")
//
//        //println("using story file: " + storyFile)
//        var storyList: List[Story] = GoldParser.parseStories(storyFile)
//        //GoldParser.parseStories(storyFile)
//
//        storyList.foreach(_.addStoryLocation())
//
//        //println("using cluster file: " + clusterFile)
//        val clusterList: List[Cluster] = initOldClusters(storyList, clusterFile)
//
//        // This filters unused sentences in the stories
//        //storyList = filterUnused(storyList, clusterList)
//        (storyList, clusterList)
//      }

//    def initOldDataFiltered(): (List[Story], List[Cluster]) =
//      {
//        val storyFile = properties.getProperty("storyFile")
//        val clusterFile = properties.getProperty("clusterFile")
//
//        //println("using story file: " + storyFile)
//        var storyList: List[Story] = GoldParser.parseStories(storyFile)
//        //GoldParser.parseStories(storyFile)
//
//        storyList.foreach(_.addStoryLocation())
//
//        //println("using cluster file: " + clusterFile)
//        val clusterList: List[Cluster] = initOldClusters(storyList, clusterFile)
//
//        // This filters unused sentences in the stories
//        storyList = filterUnused(storyList, clusterList)
//        (storyList, clusterList)
//      }

    def initData(): (List[Story], List[Cluster]) =
      {
        val storyFile = properties.getProperty("storyFile")
        val clusterFile = properties.getProperty("clusterFile")

        //println("using story file: " + storyFile)
        var storyList: List[Story] = SimpleParser.parseStories(storyFile)
        //GoldParser.parseStories(storyFile)

        storyList.foreach(_.addStoryLocation())

        println("using cluster file: " + clusterFile)
        val clusterList: List[Cluster] = initClusters(storyList, clusterFile)

        // This filters unused sentences in the stories
        //storyList = filterUnused(storyList, clusterList)
        (storyList, clusterList)
      }

    def initDataFiltered(): (List[Story], List[Cluster]) =
      {
        val storyFile = properties.getProperty("storyFile")
        val clusterFile = properties.getProperty("clusterFile")

        println("using story file: " + storyFile)
        var storyList: List[Story] = SimpleParser.parseStories(storyFile)
        //GoldParser.parseStories(storyFile)
        //println(storyList.map(_.members.map(_.toShortString()).mkString("\n")).mkString("\n###\n"))
        //System.exit(1)
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
          //val str = story.members.filterNot { s => used.contains(s) }.map { _.toShortString() }.mkString("\n")
          //println(str)
          new Story(newMembers)
        }
      }

//    def initOldClusters(storyList: List[Story], clusterFile: String) =
//      {
//        val hashmap = new HashMap[Int, Sentence]
//        storyList foreach {
//          story =>
//            story.members foreach
//              {
//                sentence =>
//                  if (hashmap.contains(sentence.id)) throw new ParsingException("sentence repeated" + sentence.id)
//                  hashmap += ((sentence.id, sentence))
//              }
//        }
//
//        GoldParser.parseClusters(clusterFile) map {
//          c =>
//            val newMembers = c.members map
//              {
//                sentence =>
//                  // make sure we get the same sentence 
//                  hashmap.get(sentence.id).get
//              }
//            val newC = new Cluster(c.name, newMembers)
//            newC.members foreach { s =>
//              s.cluster = newC
//            }
//            newC
//        }
//      }

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
                //println(sentence.id)
              }
        }

        SimpleParser.parseClusters(clusterFile) map {
          c =>
            //println("name = " + c.name)
            val newMembers = c.members map
              {
                sentence =>
                  // make sure we get the same sentence 
                  val ans = hashmap.get(sentence.id)
                  if (ans.isEmpty) throw new Exception("Did not find the same sentence in the story file: " + sentence.id)

                  ans.get
              }
            val newC = new Cluster(c.name, newMembers)
            newC.members foreach { s =>
              s.cluster = newC
            }
            newC
        }

      }

//    def graphToRelations(graph: SemanticGraph, tokens: Array[Token]): List[Dependency] = {
//      var relations = new ListBuffer[Dependency]()
//      var queue = new Queue[(IndexedWord, Int)]()
//      var used = List[IndexedWord]()
//      val root = graph.getFirstRoot()
//
//      queue += ((root, 0))
//
//      while (!queue.isEmpty) {
//        val (item, depth) = queue.dequeue()
//        val it = graph.outgoingEdgeIterable(item).iterator
//
//        while (it.hasNext) {
//          val edge = it.next()
//          val gov = edge.getGovernor()
//          val dep = edge.getDependent()
//          var relation = edge.getRelation()
//
//          // makes sure each word is added to the queue only once
//          if (!used.contains(dep)) {
//            queue += ((dep, depth + 1))
//            used = dep :: used
//          }
//
//          var specifics = relation.getSpecific()
//          if (specifics == null) specifics = ""
//          relations += new Dependency(tokens(gov.index() - 1), tokens(dep.index() - 1),
//            relation.getShortName(), specifics, depth)
//
//        }
//      }
//
//      relations.toList
//    }

    /**
     * print the sentence that does not end with a period
     *
     */
    def findIrregularSent(stories: List[Story]) {
      val temp = stories.flatMap(_.members).filterNot(s => s.tokens.last.word.trim.endsWith(".")).mkString
      println(temp)
      System.exit(0)
    }

  }

  object ConfigReader {
    def main(args: Array[String]) {
      cluster()
    }

    def clusterAll() {
      //    val string = scala.io.Source.fromFile("movieParsed.txt").mkString    
      //    val obj = XStream.fromXML(string).asInstanceOf[StorySet]
      //    println(obj.storyList.mkString("\n"))
      val reader = new ConfigReader("configNewMv.txt")
      var (mvStories, mvGold) = reader.initData()
      val minCluster = 5

      var (robStories, robGold) = new ConfigReader("configRobP.txt").initData()
      var (rtStories, rtGold) = new ConfigReader("configRtP.txt").initData()

      val mvSize = mvStories.map(_.members.size).sum
      val robSize = robStories.map(_.members.size).sum

      robStories = robStories.map { story =>
        val sents = story.members.map { s =>
          Sentence(s.id + mvSize, s.tokens)
        }
        new Story(sents)
      }

      robGold = robGold.map { gold =>
        val sents = gold.members.map { s =>
          Sentence(s.id + mvSize, s.tokens)
        }
        new Cluster("a", sents)
      }

      rtStories = rtStories.map { story =>
        val sents = story.members.map { s =>
          Sentence(s.id + mvSize + robSize, s.tokens)
        }
        new Story(sents)
      }

      rtGold = rtGold.map { gold =>
        val sents = gold.members.map { s =>
          Sentence(s.id + mvSize + robSize, s.tokens)
        }
        new Cluster("a", sents)
      }

      val stories = mvStories ::: robStories ::: rtStories
      val parser = new StoryNLPParser(stories, "AllParsed.txt", true)

      def sentFn: () => List[Sentence] = () => parser().storyList.flatMap(_.members)

      val simi = new DSDSimilarity(sentFn, "AllSemantic.txt")

      var matrix = simi()

      val (distance, max) = similarityToDistance(matrix)

      var clusterList = OPTICS.cluster(distance, max, minCluster, stories.flatMap(_.members.toList))

      val mvClusters = clusterList.map { c =>
        val realMembers = c.members.filter(s => mvStories.exists(_.members.contains(s)))
        new Cluster("a", realMembers)
      }.filter(_.members.size > 0)

      println("mv results: ")
      println(mvClusters.map(_.members.map(_.toShortString()).mkString("\n")).mkString("\n###\n"))
      evaluate(mvClusters, mvGold)

      val robClusters = clusterList.map { c =>
        val realMembers = c.members.filter(s => robStories.exists(_.members.contains(s)))
        new Cluster("a", realMembers)
      }.filter(_.members.size > 0)

      println("rob results: ")
      evaluate(robClusters, robGold)

      val rtClusters = clusterList.map { c =>
        val realMembers = c.members.filter(s => rtStories.exists(_.members.contains(s)))
        new Cluster("a", realMembers)
      }.filter(_.members.size > 0)

      println("rt results: ")
      evaluate(rtClusters, rtGold)

    }

    def cluster() {

      val dataSet = "Movie"
      var configFile = ""
      var parseFile = ""
      var semanticFile = ""
      var locationFile = ""
      var allFile = ""

      if (dataSet == "Robbery") {
        configFile = "configRobP.txt"
        parseFile = "RobParse.txt"
        semanticFile = "RobSemantic.txt"
        locationFile = "RobLocation.txt"
        allFile = "RobSimilarity.txt"
      } else if (dataSet == "Movie") {
        configFile = "configNewMvP.txt"
        parseFile = "MvParse.txt"
        semanticFile = "MvSemantic.txt"
        locationFile = "MvLocation.txt"
        allFile = "MvSimilarity.txt"
      } else if (dataSet == "Restaurant") {
        configFile = "configRtP.txt"
        parseFile = "RtParse.txt"
        semanticFile = "RtSemantic.txt"
        locationFile = "RtLocation.txt"
        allFile = "RtSimilarity.txt"
      }

      val reader = new ConfigReader(configFile)
      var (stories, gold) = reader.initData()
      val minCluster = 4
      gold = gold.filter(_.members.size >= minCluster)

      val parser = new StoryNLPParser(stories, parseFile, true)
      // val zero = s.storyList(0)
      //    println(zero)
      //    println(zero.members.mkString("\n"))

      def sentFn: () => List[Sentence] = () => parser().storyList.flatMap(_.members)

      /*// temp
      
      val sentences = sentFn()
      val verbs = sentences.flatMap(_.tokens.collect{case s:Token if s.pos == "VBD" => s.word}).distinct.sortWith(_<_)
      //println(verbs.mkString("\n")); println(verbs.size + "\n***************************")
      val nouns = sentences.flatMap(_.tokens.collect{case s:Token if s.pos == "NN" || s.pos == "NNS" => s.word}).distinct.sortWith(_<_)
      //println(nouns.mkString("\n") + "\n" + nouns.size); 
      
      val allwords = sentences.flatMap(_.tokens).map(_.word).distinct.filterNot(s => verbs.contains(s) || nouns.contains(s))
      println(allwords.mkString("\n"))
      
      System.exit(0)
      // end of temp */

      val simi = new DSDSimilarity(sentFn, semanticFile)
      //var simiMatrix = simi()
      //    utils.Matrix.prettyPrint(matrix1)
      //    System.exit(0)
      val local = new SimpleLocation(sentFn, 0.6, locationFile)

      var addition = new MatrixAddition(() => simi(), () => local(), 0.25, allFile)

      //val matrix = simi()
      var matrix = addition()
      //utils.Matrix.prettyPrint(matrix)
      //        println("sents = " + sentList.length)
      //        println("matrix length = " + matrix.length)

      //no-link constraints
      //    var count = 0
      //    for (story <- stories) {
      //      val storyLen = story.members.length
      //      for (i <- 0 until storyLen; j <- i + 1 until storyLen) {
      //        matrix(i + count)(j + count) = 0
      //        matrix(j + count)(i + count) = 0
      //      }
      //      count = storyLen
      //    }

      val (distance, max) = similarityToDistance(matrix)

      var clusterList = OPTICS.cluster(distance, max, minCluster, stories.flatMap(_.members.toList))
      //iterativeRestrain(clusterList, stories, simi())
      evaluate(clusterList, gold)
    }

    def evaluate(clusters: List[Cluster], gold: List[Cluster]) {

      val (r1, p1) = ClusterMetric.muc(gold, clusters)
      val (r2, p2) = ClusterMetric.bCubed(gold, clusters)
      println("MUC: recall " + r1 + " precision " + p1 + " f1 " + 2 * p1 * r1 / (p1 + r1))

      println("B Cubed: recall " + r2 + " precision " + p2 + " f1 " + 2 * p2 * r2 / (p2 + r2))
      println("purity: " + ClusterMetric.purity(gold, clusters))
    }

    def iterativeRestrain(cList: List[Cluster], stories: List[Story], simiMatrix: Array[Array[Double]]) {

      var clusterList = cList
      val sentences = stories.flatMap(_.members)
      writeClusters(0, clusterList)
      for (iteration <- 1 to 4) {

        val sizes = clusterList.map(_.size)
        val maxSize = sizes.max
        val minSize = sizes.min

        def weight(c: Cluster): Double = c.coherence(simiMatrix) * (c.size - minSize) / (maxSize - minSize)
        // the first N big clusters
        val big = clusterList.sortWith((c1, c2) => weight(c1) > weight(c2)).take(2 + iteration)
        println(big.mkString("\n"))

        //Thread.sleep(5000)
        val clusOrder = order(stories, big)

        //println(clusOrder.mkString("\n"))
        import scala.collection.mutable.HashMap
        var numbers = new HashMap[Cluster, Double]()
        for (i <- 0 until clusOrder.length) numbers += (clusOrder(i) -> i / (2 + iteration - 1).toDouble)

        println(numbers.mkString("\n"))

        for (story <- stories) {
          var interval = ListBuffer[Sentence]()
          var prev = 0.0
          for (sent <- story.members) {
            sent.location = 0
            val base = clusOrder.find(_.members.contains(sent))
            base match {
              case Some(c: Cluster) =>
                val cur = numbers(c)
                sent.location = numbers(c)
                println("anchor: " + sent.toShortString + ": " + sent.location)
                assignLoc(interval.toList, prev, cur)
                prev = cur
                interval.clear()
              case None => interval += sent
            }
          }
          if (!interval.isEmpty)
            assignLoc(interval.toList, prev, 1)
        }

        val betterDist = new SimpleLocation(() => sentences, 0.6 + iteration * 0.2, "movie" + iteration + "BetterLocations.txt", false)
        val addition = new MatrixAddition(() => simiMatrix, () => betterDist(), 0.2, "movie" + iteration + "Similarity.txt", false)

        val matrix = addition()

        // no-link constraints
        var count = 0
        for (story <- stories) {
          val storyLen = story.members.length
          for (i <- 0 until storyLen; j <- i + 1 until storyLen) {
            matrix(i + count)(j + count) = 0
            matrix(j + count)(i + count) = 0
          }
          count = storyLen
        }

        // no-links between all
        //    for (
        //      i <- 0 until clusterList.length;
        //      j <- i + 1 until clusterList.length
        //    ) {
        //      for (m <- clusterList(i).members)
        //      {
        //        val id1 = m.id
        //        m.location = 300
        //        for (id2 <- clusterList(j).members.map(_.id)) {
        //          matrix1(id1)(id2) = 0
        //          matrix1(id2)(id1) = 0
        //        }
        //        
        //        for(id3 <- clusterList(i).members.map(_.id)) {
        //          matrix1(id1)(id3) = matrix(id1)(id3)
        //          matrix1(id3)(id1) = matrix1(id1)(id3)  
        //        }
        //      }
        //    }

        //      
        //      var distance = matrix.map { a =>
        //        a.map { value =>
        //          if (value != 0) {
        //            val v = 1 / value
        //            if (v > max) {
        //              max = v
        //              println("smallest " + value)
        //            }
        //            v
        //          } else Double.PositiveInfinity
        //        }
        //      }

        val (distance, max) = similarityToDistance(matrix)

        //cluster.algo.OPTICS.loose = true
        clusterList = OPTICS.cluster(distance, max, 4, stories.flatMap(_.members.toList))
        writeClusters(iteration, clusterList)
      }
    }

    def similarityToDistance(matrix: Array[Array[Double]]): (Array[Array[Double]], Double) =
      {
        var max = 0.0
        for (a <- matrix; b <- a) {
          if (b > max) max = b
        }

        val top = max + 0.1

        var distance = matrix map {
          _ map { value =>
            top - value
          }
        }

        (distance, max)
      }

    def writeClusters(i: Int, clusters: List[Cluster]) {
      import java.io._
      val name = "mv-cl-" + i + ".txt"
      val clusterOut = new PrintWriter(new BufferedOutputStream(new FileOutputStream(name)))

      for (c <- clusters) {
        clusterOut.println("@ aaa")
        for (sent <- c.members) {
          clusterOut.print(sent.id + " ")
          clusterOut.println(sent.tokens.map(_.word).mkString(" "))
        }
        clusterOut.println("###")
      }

      clusterOut.close()
    }

    def assignLoc(list: List[Sentence], prev: Double, cur: Double) {
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

    //  def tempGraph(stories: List[Story], clusters: List[Cluster]) = {
    //    val reader = new ConfigReader("configMvn.txt")
    //    val para = reader.allParameters()(0)
    //    val outputPath = new File(reader.properties.getProperty("storyFile")).getParent()
    //    para.put("outputFile", outputPath + "\\conft")
    //    //Relation.init(para)
    //
    //    val gen = new GraphGenerator(stories, clusters, para)
    //    val (prevErr, prevFreedom, afterErr, afterFreedom) = gen.generate()
    //  }
    //
    //  def generateGraphs() {
    //    val reader = new ConfigReader("configMv3.txt")
    //    val (stories, clusters) = reader.initData()
    //    for (s <- stories) println(s)
    //
    //    val parameters = reader.allParameters()
    //    val outputPath = new File(reader.properties.getProperty("storyFile")).getParent()
    //    var i = 1;
    //    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(outputPath + "\\summary.csv")));
    //
    //    reader.printParameterNames(pw)
    //
    //    parameters foreach { para =>
    //      reader.printParameterValues(para, pw)
    //      para.put("outputFile", outputPath + "\\conf" + i)
    //      println(outputPath + "conf" + i)
    //      i += 1
    //
    //      //Relation.init(para)
    //      val gen = new GraphGenerator(stories, clusters, para)
    //      val (prevErr, prevFreedom, afterErr, afterFreedom) = gen.generate()
    //      pw.print(prevErr + ", " + afterErr + ", " + prevFreedom + ", " + afterFreedom + ", " + "\n")
    //
    //    }
    //  }
  }
}