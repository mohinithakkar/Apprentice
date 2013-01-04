package edu.gatech.eilab.scheherazade

import main._
import io._
import data._
import xml._
import javanlp._
import graph._
import utils.SuperProperties
import java.io._
import cluster.algo.OPTICS
import cluster.metric.ClusterMetric
import java.util.Properties
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import edu.stanford.nlp.trees.semgraph.SemanticGraph
import edu.stanford.nlp.ling.IndexedWord;

package nlp {
  object NLPMain {

    var configFile = ""
    var parseFile = ""
    var semanticFile = ""
    var locationFile = ""
    var allFile = ""

    var dataSet = "Airport"

    def main(args: Array[String]) {
      cluster()
    }

    /**
     * set the corresponding files according to the data set we are running
     *
     */
    def switchDataSet(name: String) {
      if (dataSet == "Robbery") {
        configFile = "configRob.txt"
        parseFile = "RobParse.txt"
        semanticFile = "RobSemantic.txt"
        locationFile = "RobLocation.txt"
        allFile = "RobSimilarity.txt"
      } else if (dataSet == "Movie") {
        configFile = "configNewMv.txt"
        parseFile = "MvParse.txt"
        semanticFile = "MvSemantic.txt"
        locationFile = "MvLocation.txt"
        allFile = "MvSimilarity.txt"
      } else if (dataSet == "Restaurant") {
        configFile = "configRt.txt"
        parseFile = "RtParse.txt"
        semanticFile = "RtSemantic.txt"
        locationFile = "RtLocation.txt"
        allFile = "RtSimilarity.txt"
      } else if (dataSet == "Airport") {
        configFile = "configAir.txt"
        parseFile = "AirParse.txt"
        semanticFile = "AirSemantic.txt"
        locationFile = "AirLocation.txt"
        allFile = "AirSimilarity.txt"
      }
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
      switchDataSet("Airport")
      val reader = new ConfigReader(configFile)
      var (stories, gold) = reader.initData()
      val minCluster = 4
      gold = gold.filter(_.members.size >= minCluster)

      val parser = new StoryNLPParser(stories, parseFile, true)
      // val zero = s.storyList(0)
      //    println(zero)
      //    println(zero.members.mkString("\n"))

      def sentFn: () => List[Sentence] = () => parser().storyList.flatMap(_.members)

      /*
      val sentences = sentFn()
      //      val verbs = sentences.flatMap(_.tokens.collect{case s:Token if s.pos == "VBD" => s.word}).distinct.sortWith(_<_)
      //      //println(verbs.mkString("\n")); println(verbs.size + "\n***************************")
      //      val nouns = sentences.flatMap(_.tokens.collect{case s:Token if s.pos == "NN" || s.pos == "NNS" => s.word}).distinct.sortWith(_<_)
      //      //println(nouns.mkString("\n") + "\n" + nouns.size); 
      //      
      //      val allwords = sentences.flatMap(_.tokens).map(_.word).distinct.filterNot(s => verbs.contains(s) || nouns.contains(s))
      //      println(allwords.mkString("\n"))
      //      
      sentences.foreach(s =>
        println(s.toShortString()))
      System.exit(0)
      // end of temp */

      val simi = new DSDSimilarity(sentFn, semanticFile)
      /* temp code here
      var simiMatrix = simi()
      val pw = new java.io.PrintWriter(new FileOutputStream(new File("simMatrix.txt")))
          utils.Matrix.prettyPrintTo(pw,simiMatrix, 6)
          pw.close();
          System.exit(0)
        */
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

      matrix = mutualKNN(matrix, 7);
      val (distance, max) = similarityToDistance(matrix)

      var clusterList = OPTICS.cluster(distance, max, minCluster, stories.flatMap(_.members.toList))
      //iterativeRestrain(clusterList, stories, simi())
      evaluate(clusterList, gold)
      
      
    }

    def mutualKNN(matrix: Array[Array[Double]], k: Int): Array[Array[Double]] =
      {
        val n = matrix.size
        val answer = matrix.clone()
        for (i <- 0 until n) {
          val sorted = answer(i).sortWith(_ > _);
          val cutoff = sorted(k);
          for (j <- 0 until n) {
            if (answer(i)(j) < cutoff) answer(i)(j) = 0
          }
        }

        for (i <- 0 until n; j <- 0 until n)
          answer(i)(j) = math.min(answer(i)(j), answer(j)(i));

        answer
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

  }
}