package edu.gatech.eilab.scheherazade
import data._
import java.io._
import javanlp._
import scala.collection.mutable.ListBuffer
import edu.stanford.nlp.trees.semgraph.SemanticGraph
import edu.stanford.nlp.ling.IndexedWord;

package main {
  abstract class CachedOp[T](val cacheFile: String, val saveFile: String, val overWrite: Boolean = true, val compression: Int = 0, val cached: Boolean = true) {

    final val COMPRESSED = 0;
    final val PLAIN_TEXT = 1;
    final val BOTH = 2;

    def read(): Option[T] =
      {
        try {

          val string =
            {
              val plainFile = new File(cacheFile)
              if (plainFile.exists()) {
                if (cacheFile.endsWith(".lzma")) read7z(plainFile)
                else
                  scala.io.Source.fromFile(plainFile).mkString
              } else {
                val lzmaFile = new File(cacheFile + ".lzma")
                if (lzmaFile.exists()) {
                  read7z(lzmaFile)
                } else
                  return None
              }
            }

          val obj = XStream.fromXML(string).asInstanceOf[T]
          return Some(obj)
        } catch {
          case ex: IOException =>
            println("XML Reading Error: " + ex.getClass().getName() + " " + ex.getMessage())
            ex.printStackTrace()
            None
        }
      }

    def compute(): T

    def apply(): T = {

      if (cached) {
        var updated = false

        val result =
          read() match {
            case s: Some[T] => s.get.asInstanceOf[T]
            case None => {
              updated = true
              compute()
            }
          }

        val save = new File(saveFile)
        if (updated && (overWrite || (!save.exists))) {
          val str = XStream.toXML(result)

          if (compression == PLAIN_TEXT || compression == BOTH) {
            val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(save)))
            pw.println(str)
            pw.close()
          }

          if (compression == COMPRESSED || compression == BOTH) {
            write7z(save + ".lzma", str)
          }
        }
        result
      } else compute()
    }

    def read7z(file: File): String = {
      val byteStream = new FileInputStream(file)
      println("reading lzma stream from : " + file.getName())
      val length = byteStream.available()
      val b = new Array[Byte](length)
      byteStream.read(b)
      byteStream.close()
      val string = data.SevenZip.decode(b)
      string
    }

    def read7z(filename: String): String = {
      val byteStream = new FileInputStream(filename)
      val length = byteStream.available()
      val b = new Array[Byte](length)
      byteStream.read(b)
      byteStream.close()
      val string = data.SevenZip.decode(b)
      string
    }

    def write7z(filename: String, text: String) {
      val out = new BufferedOutputStream(new FileOutputStream(filename))
      val bytes = data.SevenZip.encode(text)
      //    println(bytes.mkString(" "))
      println("writing to " + filename)
      out.write(bytes)
      out.close()
    }
  }

  class StoryNLPParser(val storyList: List[Story], cacheFile: String, overWrite: Boolean = true) extends CachedOp[StorySet](cacheFile, cacheFile, overWrite) {
    def compute(): StorySet = {

      val text = storyList.flatMap { _.members.map { _.tokens.map(_.word).mkString(" ") } }.mkString("\n")
      val nlp = new NLPWrapper()
      println("parsing: ************************************")
      println(text)
      println("parsed: ************************************")
      nlp.getParsed(text)
      val newStories = storyList map { story =>
        val newSents = story.members map { sent =>

          if (!nlp.hasNextSentence()) throw new RuntimeException("parsed sentence exhausted prematurely")
          nlp.processNextSentence();

          var tokensArray = nlp.getTokens()
          if (tokensArray.length > 1 || tokensArray(0)(0) != ".") { // filters out empty sentences with a single period

            var tokenBuffer = new ListBuffer[Token]()
            for (i <- 0 to tokensArray.length - 1) {
              val t = tokensArray(i)
              tokenBuffer += new Token(i, t(0), t(1), t(2), { if (t(3) == "O") "" else t(3) })
            }

            val tokens = tokenBuffer.toArray

            val tree = nlp.getParseTree()
            val graph = nlp.getSemanticGraph()

            val relations = graphToRelations(graph, tokens)
            println("parsed: " + sent.id + " " + tokens.map(x => x.word).mkString(" "))
            Sentence(sent.id, tokens, null, relations, sent.location)
          } else throw new RuntimeException("empty sentence " + sent.id)
        }
        new Story(newSents)
      }

      new StorySet(cacheFile, newStories)
    }

    /**
     * convert the Standford Semantic Graph to a list of relations
     *
     */
    def graphToRelations(graph: SemanticGraph, tokens: Array[Token]): List[Dependency] = {

      import scala.collection.mutable.Queue

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
  }

  class DSDSimilarity(sentFn: () => List[Sentence], cacheFile: String, overWrite: Boolean = true) extends CachedOp[Array[Array[Double]]](cacheFile, cacheFile, overWrite) {
    def compute(): Array[Array[Double]] = {

      val sentList: List[Sentence] = sentFn()
      val sim = new SimilarityMetric()
      val matrix = Array.fill(sentList.length, sentList.length)(0.0)
      // do the preprocessing once and for all
      val sents = sentList.map { s =>
        val n = new Sentence(s.id, s.tokens, s.parse, sim.preprocess(s.deps), s.location)
        //      val v = sim.preprocess(s.deps)
        //      if (v != s.deps) println(s.toShortString + "\n" + v.mkString("\n") + "\n\n")
        n
      }

      //System.exit(-0)
      for (i <- 0 until sents.length) {
        for (j <- i + 1 to sents.length - 1) {
          sim.sentenceSimilarity(sents(i), sents(j))._1
        }
      }

      sim.normalize()

      for (i <- 0 until sents.length) {
        for (j <- i + 1 to sents.length - 1) {
          matrix(i)(j) = sim.sentenceSimilarity(sents(i), sents(j))._1
          matrix(j)(i) = matrix(i)(j)
          //if (matrix(i)(j) > 1) throw new Exception(sents(i).toString() + " " + sents(j).toString + " " + matrix(i)(j)) 
        }
      }

      matrix
    }
  }

  class SimpleLocation(sentFn: () => List[Sentence], locWeights: Double, cacheFile: String, cached: Boolean = true, overWrite: Boolean = true) extends CachedOp[Array[Array[Double]]](cacheFile, cacheFile, overWrite, 0, cached) {
    //final val LOC_WTS = 0.8

    def compute(): Array[Array[Double]] = {
      val sentList = sentFn()
      val length = sentList.length
      val matrix = Array.ofDim[Double](length, length)
      for (
        i <- 0 until length;
        j <- i + 1 until length
      ) {
        val sent1 = sentList(i)
        val sent2 = sentList(j)
        //println(sent1.location + " // " + sent2.location) 
        var value = (locWeights / 2) - locWeights * math.abs(sent1.location - sent2.location)
        if (value < 0.01) value = 0
        matrix(i)(j) = value
        matrix(j)(i) = value
      }
      matrix
    }
  }

  class MatrixAddition(matrix1: () => Array[Array[Double]], matrix2: () => Array[Array[Double]], filter: Double, cacheFile: String, cached: Boolean = true, overWrite: Boolean = true) extends CachedOp[Array[Array[Double]]](cacheFile, cacheFile, overWrite, 0, cached) {
    def compute(): Array[Array[Double]] = {
      val m1 = matrix1()
      val m2 = matrix2()
      val length = m1.length
      if (m1(0).length != length || m2.length != length || m2(0).length != length)
        throw new RuntimeException("matrix sizes do not match: " + m1(0).length + " " + m2(0).length)

      val result = Array.ofDim[Double](length, length)
      for (i <- 0 until length; j <- 0 until length) {
        result(i)(j) = m1(i)(j) + m2(i)(j)
        if (result(i)(j) < filter) result(i)(j) = 0
      }

      result
    }

  }
}