package comparison
import scala.util.parsing.combinator._
import parse._
import data._
import cluster.metric.ClusterMetric

case class Parameters(val verb: Double, val noun: Double, val location: Double)
case class Result(val precision: Double, val recall: Double, val f1: Double)

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

    for (i <- 0 to 216) {
      val doc = scala.io.Source.fromFile("./comparison data/data3/result_" + i + ".txt")
      val it = doc.getLines
      val head = it.next() + "\n" + it.next()
      val rest = it.mkString("\n").replaceAll(" /.", "")
      val (p: Parameters, r: Result) = parseAll(header, head).get

      val clusterFile = "GoldRebuttal.txt"

      var storyList: List[Story] = GoldParser.parseStoryText(rest)

      val clusterList: List[Cluster] = GoldParser.parseClusters(clusterFile)

      val results = storyList.map(s => new Cluster("a", s.members.toList))
      val (r1, p1) = ClusterMetric.muc(clusterList, results)
      val (r2, p2) = ClusterMetric.bCubed(clusterList, results)

      print(p.verb + ", " + p.noun + ", " + p.location)
      print(",Original, " + r.precision + ", " + r.recall + ", " + r.f1)
      ///print(", , ,")
      print(",MUC, " + p1 + ", " + r1 + ", " + 2 * p1 * r1 / (p1 + r1))
      ///print(", , ,")
      println(",B Cubed, " + p2 + ", " + r2 + ", " + 2 * p2 * r2 / (p2 + r2))
    }
  }
}