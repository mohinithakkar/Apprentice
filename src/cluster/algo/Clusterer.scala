package cluster.algo

import parse._
import data._

/**
 * The hierarhical clustering algorithm
 * @author Albert Li
 */
object HierarchicalClusterer {

  def main(args: Array[String]) {
    var matrix = readCSV("movieOriginalSimilarity.csv")
    var sentences: Array[Sentence] = SimpleParser.parseStories("./data/movie/movieSimpleStories.txt").flatMap(_.members).toArray
    sentences = sentences.map{
        s => 
          val nt = s.tokens.map(t => Token(t.word, "AB"))
          Sentence(s.id, nt, null, Nil)
          }
    val cl: List[List[Int]] = cluster(matrix)

    println("final clusters = ")
    cl foreach {
      list =>
        list foreach { index =>
          val s = sentences(index)
          val str = s.toString
          println(s.id + " " + str.substring(str.indexOf(")") + 2))
        }

        println("###")
    }
  }

  def readCSV(filename: String): Array[Array[Double]] =
    {
      val text = scala.io.Source.fromFile(filename).mkString
      val matrix =
        text.split("\n").map {
          line =>
            line.split(",").filterNot { _.trim == "" }.map {
              item => item.trim.toDouble
            }
        }

      for (i <- 0 until matrix.length; j <- 0 until i)
        matrix(i)(j) = matrix(j)(i)

      // use a threshold to filter the similarity values
      for (i <- 0 until matrix.length; j <- 0 until matrix.length)
        if (matrix(i)(j) < 0.6) matrix(i)(j) = 0

      for (i <- 0 to 20) {
        for (j <- 0 to 20)
          print(("%.2f" format matrix(i)(j)) + ", ")
        println
      }
      matrix
    }

  def cluster(similarity: Array[Array[Double]]): List[List[Int]] =
    {
      var list = (0 to similarity.length - 1).map(List(_)).toList
      var singles = list.filter { l => !similarity(l(0)).exists(_ != 0) }
      list = list -- singles

      println("single sentences: " + singles.flatten.mkString(" "))
      var merge = (List(0), List(0), 0.0)
      do {

        var pairs = productNonEqual(list, list).filter { x => x._1 != x._2 }.map { x => (x._1, x._2, averageLink(x._1, x._2, similarity)) }
        //println(pairs)
        var sorted = pairs.sort { (x, y) => x._3 > y._3 }
        //println(sorted(0) + " " + sorted(1))
        merge = sorted(0)
        list = (merge._1 ::: merge._2) :: (list - merge._1 - merge._2)
        //println("merged " + merge._1 + " and " + merge._2)

      } while (list.length > 68 && merge._3 > 0.6)
      println("current list length = " + list.length)
      list
    }

  /* definition of complete link?
   * 
   */
  def averageLink(a: List[Int], b: List[Int], similarity: Array[Array[Double]]): Double =
    {
      val pairs = productNonEqual(a, b)
      val sim = pairs map {
        x =>
          assert(x._1 != x._2)
          similarity(x._1)(x._2)
      }

      sim.foldRight(0.0) { (x, y) => x + y } / pairs.length
    }

  /* definition of complete link?
   * 
   */
  def completeLink(a: List[Int], b: List[Int], similarity: Array[Array[Double]]): Double =
    {
      val pairs = productNonEqual(a, b)
      val sim = pairs map {
        x =>
          assert(x._1 != x._2)
          similarity(x._1)(x._2)
      }

      sim.foldRight(100.0) { (x, y) => if (x < y) x else y }
    }

  def productNonEqual[T](a: List[T], b: List[T]): List[(T, T)] =
    {
      var result = List[(T, T)]()
      for (a1 <- a)
        for (b1 <- b)
          if (a1 != b1) result = ((a1, b1)) :: result

      result
    }
}