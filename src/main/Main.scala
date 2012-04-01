package main
import data._
import scala.xml._
import java.io._

object Main {

//  def main(args: Array[String]) {
//    //val initializer = new ConfigReader("runconfig.txt")
//    var sents = readSentences("movieParse.xml")
//    //    val text = sents.map(_.tokens.map(_.word).mkString(" ")).mkString("\n")
//    println("read " + sents.length + " sentences, from " + sents(0).id + " to " + sents(sents.length - 1).id)
//    val sim = new SimilarityMetric()
//    val matrix = Array.fill(sents.length, sents.length)(0.0)
//
//    //println(sents.map { _.toShortString }.mkString("\n"))
//    
//    // do the preprocessing once and for all
//    sents = sents.map{s => Sentence(s.id, s.tokens, s.parse, sim.preprocess(s.deps), s.location)}
//    
//    for (i <- 0 to sents.length - 1) {
//      println("processing: " + i)
//      for (j <- i + 1 to sents.length - 1) {
//        matrix(i)(j) = sim.sentenceSimilarity(sents(i), sents(j))._1
//      }
//    }
//    val filename = "movieOriginalSimilarity.csv"
//    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(filename)))
//    for (i <- 0 to sents.length - 1) {
//      
//      for (j <- 0 to sents.length - 1) {
//        pw.print(("%.6f" format matrix(i)(j)) + ", ")
//      }
//      pw.println()
//    }
//    
//    pw.close()
//    println("finished printing to file: " + filename)
//  }

//  def readSentences(filename: String): Array[Sentence] =
//    {
//      val text: String = scala.io.Source.fromFile(filename).mkString
//      val xml = XML.loadString(text)
//      //println(xml)
//      val senNodes = xml \\ "Sentence"
//      //println("sents = " + senNodes)
//
//      val senSeq = senNodes.map { sen =>
//        //print("iter")
//        val id = (sen \ "id" text) toInt
//        val tokenNodes = sen \\ "Token"
//        val tt = tokenNodes.map { t =>
//          {
//            val id = (t \ "id" text) toInt
//            val word = (t \ "word").text
//            val lemma = (t \ "lemma").text
//            val pos = (t \ "pos").text
//            val ner = if ((t \ "ner").text.equals("O"))
//              ""
//            else
//              (t \ "ner").text
//
//            //println("captured token id" + id)
//            new Token(id, word, pos, lemma, ner)
//          }: Token
//        }
//
//        val tokens = tt.toArray
//        //println(tokens.mkString("", " ", "\n"))
//
//        val depNodes = sen \\ "Dependency"
//        //println(depNodes)
//        val deps = depNodes.map { t =>
//          //println(t)
//          val relation = (t \ "relation").text
//          val specific = (t \ "specific").text
//          val govID = (t \ "governorId").text.toInt
//          val depID = (t \ "dependentId").text.toInt
//          val depth = (t \ "depth").text.toInt
//          //println("rel : " + rel + govID + " " + depID)
//
//          new Dependency(tokens(govID), tokens(depID), relation, specific, depth)
//        }.toList
//
//        val newSentence = Sentence(id, tokens, null, deps)
//
//        newSentence
//      }
//
//      senSeq.toArray
//    }
}