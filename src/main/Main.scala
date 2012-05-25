package main
import data._
import scala.xml._
import graph._
import java.io._

object Main extends App {

  generateGraphs()
  //countClusterFreq()
  Thread.sleep(5000)
  
  def countClusterFreq()
  {
    val reader = new ConfigReader("configMvS.txt")
    val (stories, clusters) = reader.initDataFiltered()
    val sorted = clusters.sortWith((a, b) => a.members.length > b.members.length)
    val text = sorted.map(x => x.name + ": " + x.members.length).mkString("\n")
    println(text)
  }
  
  def generateGraphs() {
    val reader = new ConfigReader("configMvS.txt")
    val (stories, clusters) = reader.initDataFiltered()
    //for (s <- stories) println(s)

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
    pw.close()
  }
}