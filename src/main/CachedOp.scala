package main
import data._
import java.io._
import javanlp._
import scala.collection.mutable.ListBuffer
import edu.stanford.nlp.trees.semgraph.SemanticGraph
import edu.stanford.nlp.ling.IndexedWord;

abstract class CachedOp[T <: XStreamable](val cacheFile: String, val saveFile: String, val overWrite: Boolean = true, val compression: Int = 0) {

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

    var updated = false

    val result =
      read() match {
        case s: Some[T] => s.get
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
  }

  def read7z(file: File): String = {
    val byteStream = new FileInputStream(file)
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
    println(bytes.mkString(" "))
    println("writing to " + filename)
    out.write(bytes)
    out.close()
  }
}

class StoryNLPParser(val storyList: List[Story], cacheFile: String, overWrite: Boolean = true) extends CachedOp[StorySet](cacheFile, cacheFile, overWrite) {
  def compute(): StorySet = {

    val text = storyList.flatMap { _.members.map { _.tokens.map(_.word).mkString(" ") } }.mkString("\n")
    val nlp = new NLPWrapper()
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
          Sentence(sent.id, tokens, null, relations)
        } else throw new RuntimeException("empty sentence ")
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