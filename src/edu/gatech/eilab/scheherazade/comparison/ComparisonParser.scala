package edu.gatech.eilab.scheherazade
import scala.util.parsing.combinator._
import parse._
import data._
import cluster.metric.ClusterMetric
package comparison {
  case class Parameters(val verb: Double, val noun: Double, val location: Double)
  case class Result(val precision: Double, val recall: Double, val f1: Double) extends Ordered[Result] {
    override def compare(that: Result): Int = {
      if (this.f1 > that.f1) 1
      else if (this.f1 == that.f1) 0
      else -1
    }

    def +(that: Result) = Result(this.precision + that.precision, this.recall + that.recall, this.f1 + that.f1)

    def /(divisor: Double) = Result(precision / divisor, recall / divisor, f1 / divisor)

    override def toString() = "precision = " + precision + " recall = " + recall + " f1 = " + f1
  }

  object ComparisonParser extends JavaTokenParsers {
    def paraLine: Parser[Parameters] = "verbW:" ~> decimalNumber ~ "nounW:" ~ decimalNumber ~ "locationW:" ~ decimalNumber ^^ {
      case n1 ~ "nounW:" ~ n2 ~ "locationW:" ~ n3 => Parameters(n1.toDouble, n2.toDouble, n3.toDouble)
    }

    def resultLine: Parser[Result] = "P" ~> decimalNumber ~ "R" ~ decimalNumber ~ "F1" ~ decimalNumber ^^ {
      case n1 ~ "R" ~ n2 ~ "F1" ~ n3 => Result(n1.toDouble, n2.toDouble, n3.toDouble)
    }

    def header: Parser[(Parameters, Result)] = paraLine ~ resultLine ^^ {
      case p ~ r => (p, r)
    }

    def main(args: Array[String]) {
      //    if (args.length < 2) {
      //      println("needs two parameters")
      //      System.exit(-1)
      //    }
      //
      //    val func = args(0).toInt
      //    val path = args(1)
      //    
      //    func match {
      //      case 1 => function1(path)
      //      case 2 => function2(path)
      //      case 3 => function3(path)
      //    }

      function1()
    }

    // average results
    def function3(path: String = "./comparison data/rest_initial_final/") {

      var mucSum = Result(0, 0, 0)
      var homeSum = Result(0, 0, 0)
      var bSum = Result(0, 0, 0)

      for (j <- 1 to 10) {
        val folder = "results" + j

        for (i <- 0 to 9) {
          val doc = scala.io.Source.fromFile(path + folder + "/result_" + i + ".txt")
          val it = doc.getLines
          val head = it.next() + "\n" + it.next()
          val rest = it.mkString("\n").replaceAll(" /.", "").replaceAll("’", "")
          val (p: Parameters, homeResult: Result) = parseAll(header, head).get

          //val clusterFile = "GoldRebuttal.txt"
          val clusterFile = "testGoldRestaurant.txt"

          var storyList: List[Story] = GoldParser.parseStoryText(rest)

          val clusterList: List[Cluster] = GoldParser.parseClusters(clusterFile)

          val results = storyList.map(s => new Cluster("a", s.members.toList))
          val (r1, p1) = ClusterMetric.muc(clusterList, results)
          val (r2, p2) = ClusterMetric.bCubed(clusterList, results)

          val f1 = 2 * p1 * r1 / (p1 + r1)
          val f2 = 2 * p2 * r2 / (p2 + r2)

          val mucResult = Result(p1, r1, f1)
          val b3Result = Result(p2, r2, f2)

          mucSum = mucSum + mucResult
          homeSum = homeSum + homeResult
          bSum = bSum + b3Result
        }

      }

      println("MUC = " + mucSum / 100)
      println("Home = " + homeSum / 100)
      println("B^3 = " + bSum / 100)
    }

    // finding the parameters that has the best results on average
    def function2(path: String = "./comparison data/restaurant/") {
      import scala.collection.mutable.HashMap

      val best = new HashMap[Parameters, (Double, Int)]()

      for (j <- 1 to 10) {
        val folder = "results" + j

        for (i <- 0 to 216) {
          val doc = scala.io.Source.fromFile(path + folder + "/result_" + i + ".txt")
          val it = doc.getLines
          val head = it.next() + "\n" + it.next()
          val rest = it.mkString("\n").replaceAll(" /.", "").replaceAll("’", "")
          val (p: Parameters, r: Result) = parseAll(header, head).get

          //val clusterFile = "GoldRebuttal.txt"
          val clusterFile = "testGoldRestaurant.txt"

          var storyList: List[Story] = GoldParser.parseStoryText(rest)

          val clusterList: List[Cluster] = GoldParser.parseClusters(clusterFile)

          val results = storyList.map(s => new Cluster("a", s.members.toList))
          val (r1, p1) = ClusterMetric.muc(clusterList, results)
          val (r2, p2) = ClusterMetric.bCubed(clusterList, results)

          val f1 = 2 * p1 * r1 / (p1 + r1)

          val f2 = 2 * p2 * r2 / (p2 + r2)

          val newResult = Result(p1, r1, f1)
          if (best.contains(p)) {
            val old = best(p)
            best += (p -> (f1 + old._1, old._2 + 1))
          } else {
            best += (p -> (f1, 1))
          }

          print(".")

        }
        println()
      }

      val avg = best.toList.map(x => (x._1, x._2._1 / x._2._2))

      val zero = avg.filter(_._1.location == 0).sortBy(_._2).reverse.mkString("\n")

      println(zero)
      val nonzero = avg.filterNot(_._1.location == 0).sortBy(_._2).reverse.mkString("\n")
      println()
      println(nonzero)
    }

    // average of the best of each parameter sweep
    def function1(path: String = "./comparison data/rest_final/") {
      // these are for column 2: location weights == 0
      var mucSum2 = Result(0, 0, 0)
      var homeSum2 = Result(0, 0, 0)
      var bSum2 = Result(0, 0, 0)

      // these are for column 3: location weights != 0
      var mucSum3 = Result(0, 0, 0)
      var homeSum3 = Result(0, 0, 0)
      var bSum3 = Result(0, 0, 0)

      import java.io._
      //val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File("muc_nv.csv"))))

      for (j <- 1 to 10) {
        val folder = "results" + j

        var bestMUC2 = Result(0, 0, 0)
        var bestHome2 = Result(0, 0, 0)
        var bestB32 = Result(0, 0, 0)

        var bestMUC3 = Result(0, 0, 0)
        var bestHome3 = Result(0, 0, 0)
        var bestB33 = Result(0, 0, 0)

        for (i <- 0 to 216) {
          val doc = scala.io.Source.fromFile(path + folder + "/result_" + i + ".txt")
          val it = doc.getLines
          val head = it.next() + "\n" + it.next()
          val rest = it.mkString("\n").replaceAll(" /.", "").replaceAll("’", "")
          val (p: Parameters, homeResult: Result) = parseAll(header, head).get

          //val clusterFile = "GoldRebuttal.txt"
          val clusterFile = "testGoldRestaurant.txt"

          var storyList: List[Story] = GoldParser.parseStoryText(rest)

          val clusterList: List[Cluster] = GoldParser.parseClusters(clusterFile)

          val results = storyList.map(s => new Cluster("a", s.members.toList))
          val (r1, p1) = ClusterMetric.muc(clusterList, results)
          val (r2, p2) = ClusterMetric.bCubed(clusterList, results)

          val f1 = 2 * p1 * r1 / (p1 + r1)

          val mucResult = Result(p1, r1, f1)
          val f2 = 2 * p2 * r2 / (p2 + r2)

          val b3Result = Result(p2, r2, f2)

          if (p.location == 0) {
            // belongs to column 2
            if (mucResult > bestMUC2) {
              bestMUC2 = mucResult
            }
            if (homeResult > bestHome2) {
              bestHome2 = homeResult
            }
            if (b3Result > bestB32) {
              bestB32 = b3Result
            }
          } else {
            // belongs to column 3
            if (mucResult > bestMUC3) {
              bestMUC3 = mucResult
            }
            if (homeResult > bestHome3) {
              bestHome3 = homeResult
            }
            if (b3Result > bestB33) {
              bestB33 = b3Result
            }
          }

          print(".")

          //pw.print(p.verb + ", " + p.noun + ", " + p.location)
          // pw.print(",Original, " + r.precision + ", " + r.recall + ", " + r.f1)
          ///print(", , ,")
          //pw.print(",MUC, " + p1 + ", " + r1 + ", " + 2 * p1 * r1 / (p1 + r1))
          ///print(", , ,")
          //pw.println(",B Cubed, " + p2 + ", " + r2 + ", " + 2 * p2 * r2 / (p2 + r2))
        }

        homeSum2 = homeSum2 + bestHome2
        homeSum3 = homeSum3 + bestHome3
        mucSum2 = mucSum2 + bestMUC2
        mucSum3 = mucSum3 + bestMUC3
        bSum2 = bSum2 + bestB32
        bSum3 = bSum3 + bestB33
        println()

        //println("\ncol2 f = " + bestF2 + " col3 f = " + bestF3)
      }
      //pw.close()
      println()
      println("Column 2:")
      println("MUC = " + mucSum2 / 10)
      println("Home = " + homeSum2 / 10)
      println("B^3 = " + bSum2 / 10)
      println("Column 3:")
      println("MUC = " + mucSum3 / 10)
      println("Home = " + homeSum3 / 10)
      println("B^3 = " + bSum3 / 10)
    }
  }
}