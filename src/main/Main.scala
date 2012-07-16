package main
import data._
import scala.xml._
import graph._
import java.io._
import collection.mutable.ListBuffer

object Main extends App {

  generateGraphs()
  //countClusterFreq()
  Thread.sleep(10000)

  def countClusterFreq() {
    val reader = new ConfigReader("configNewMv.txt")
    val (stories, clusters) = reader.initDataFiltered()
    val sorted = clusters.sortWith((a, b) => a.members.length > b.members.length)
    val text = sorted.map(x => x.name + ": " + x.members.length).mkString("\n")
    println(text)
  }

  def generateGraphs() {
    val reader = new ConfigReader("configNewMv.txt")
    val (stories, clusters) = reader.initData()
    //val (stories, clusters) = reader.initOldDataFiltered()
    //for (s <- stories) println(s)

    val property = reader.properties
    val parameters = property.allParameters()

    val outputPath = new File(reader.properties.getProperty("storyFile")).getParent()
    var i = 1;
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(outputPath + "\\summary.csv")));

    property.printParameterNames(pw)

    var prevErrList = ListBuffer[Double]()
    var afterErrList = ListBuffer[Double]()

    parameters foreach { para =>

      // filter out clusters smaller than the minimum cluster size
      val minimumSize = para.getParameter("minClusterSize", text => text.trim.toInt).getOrElse(0)
      val insideClusters = clusters.filterNot(c => c.members.size < minimumSize)
      val insideStories = reader.filterUnused(stories, insideClusters)

      para.printParameterValues(pw)
      para.put("outputFile", outputPath + "\\conf" + i)
      println(outputPath + "conf" + i)
      i += 1

      val gen = new GraphGenerator(insideStories, insideClusters, para)
      val (beforeErr, beforeGraph, afterErr, afterGraph) = gen.generate()
      if (afterGraph != null) {
        
        val beforeCoverage = rateEntireCoverage3(stories, beforeGraph)
        val afterCoverage = rateEntireCoverage3(stories, afterGraph)
        pw.print(beforeErr + ", " + afterErr + "," + ((1 - (afterErr / beforeErr)) * 100) + "," + beforeCoverage + "," + afterCoverage + "\n")
        prevErrList += beforeErr
        afterErrList += afterErr
      }
      else
      {
        // the graph contains loops
        pw.println("loop detected")
      }

    }

    val emptyCols = parameters.head.stringPropertyNames.size()
    pw.println(" ," * emptyCols + (prevErrList.sum / prevErrList.size) + ", " + (afterErrList.sum / afterErrList.size))
    pw.close()
  }

  private def rateEntireCoverage3(stories: List[Story], graph: Graph): Double =
    {
      val egraph: EfficientGraph = if (!graph.isInstanceOf[EfficientGraph]) graph.makeEfficient() else graph.asInstanceOf[EfficientGraph]

      var topScore = 0.0
      var bottomScore = 0
      var seq = ListBuffer[Double]()
      stories map {
        story =>

          //println("!!contains " + score + "valid sentences out of " + n + "!")

          val pairs = for (i <- 0 until story.size; j <- i + 1 until story.size if story.members(i).cluster != null && story.members(j).cluster != null) yield {
            val source = story.members(i).cluster
            val target = story.members(j).cluster
            (source, target)
          }

          val pass = pairs.forall {
            case (source, target) =>
              (egraph.nodes.contains(source) && egraph.nodes.contains(target) &&
                egraph.shortestDistance(target, source) == -1)
          }

          topScore += {
            if (pass) {
              val n = pairs.size
              val c = pairs.filter {
                case (source, target) =>
                  egraph.shortestDistance(source, target) != -1
              }.size
              if (n == 0) 0
              else c / n
            } else 0
          }

      }
      topScore.toDouble / stories.size
    }

  private def rateEntireCoverage2(stories: List[Story], graph: Graph): Double =
    {
      val egraph: EfficientGraph = if (!graph.isInstanceOf[EfficientGraph]) graph.makeEfficient() else graph.asInstanceOf[EfficientGraph]

      var topScore = 0
      var bottomScore = 0
      var seq = ListBuffer[Double]()
      stories map {
        story =>
          var score = 0
          //println("!!contains " + score + "valid sentences out of " + n + "!")

          var pairs = for (i <- 0 until story.size; j <- i + 1 until story.size if story.members(i).cluster != null && story.members(j).cluster != null) yield {
            val source = story.members(i).cluster
            val target = story.members(j).cluster
            (source, target)
          }
          val n = pairs.size
          var valid = true

          while (valid && pairs != Nil) {
            var (source, target) = pairs.head
            pairs = pairs.tail

            if (egraph.nodes.contains(source) && egraph.nodes.contains(target) &&
              egraph.shortestDistance(target, source) == -1)
              score += 1
            else valid = false
          }

          seq += score.toDouble / n

        //println("score, total = " + score + ", " + total)

      }
      seq.sum / seq.size
    }

  private def rateEntireCoverage(stories: List[Story], graph: Graph): Double =
    {
      val egraph: EfficientGraph = if (!graph.isInstanceOf[EfficientGraph]) graph.makeEfficient() else graph.asInstanceOf[EfficientGraph]

      var topScore = 0
      var bottomScore = 0
      var seq = ListBuffer[Double]()
      stories map {
        story =>

          //println("!!contains " + score + "valid sentences out of " + n + "!")

          val pairs = for (i <- 0 until story.size; j <- i + 1 until story.size if story.members(i).cluster != null && story.members(j).cluster != null) yield {
            val source = story.members(i).cluster
            val target = story.members(j).cluster
            (source, target)
          }

          val pass = pairs.forall {
            case (source, target) =>
              (egraph.nodes.contains(source) && egraph.nodes.contains(target) &&
                egraph.shortestDistance(target, source) == -1)
          }

          topScore += { if (pass) 1 else 0 }

        //println("score, total = " + score + ", " + total)

      }
      topScore.toDouble / stories.size
    }

  private def rateCoverage(stories: List[Story], graph: Graph): Double =
    {
      val egraph: EfficientGraph = if (!graph.isInstanceOf[EfficientGraph]) graph.makeEfficient() else graph.asInstanceOf[EfficientGraph]

      var numerator = 0
      var denominator = 0
      var seq = ListBuffer[Double]()
      stories map {
        story =>
          // total points = n + n-choose-2, where n is number of sentences
          val n = story.size
          val total = n + n * (n - 1) / 2
          var score = 0
          for (sent <- story.members) {
            if (sent.cluster != null && graph.nodes.contains(sent.cluster))
              score += 1
          }

          //println("!!contains " + score + "valid sentences out of " + n + "!")

          for (i <- 0 until story.size; j <- i + 1 until story.size if story.members(i).cluster != null && story.members(j).cluster != null) {
            val source = story.members(i).cluster
            val target = story.members(j).cluster
            if (egraph.nodes.contains(source) && egraph.nodes.contains(target) &&
              egraph.shortestDistance(target, source) == -1)
              score += 1
          }

          //println("score, total = " + score + ", " + total)

          numerator += score
          denominator += total
        //seq += score.toDouble / total
      }
      //seq.sum / seq.size
      numerator.toDouble / denominator
    }
}