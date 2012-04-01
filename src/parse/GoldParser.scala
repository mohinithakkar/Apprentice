package parse

import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap
import java.io._
import data._
import data._

object GoldParser extends JavaTokenParsers {
  protected def word: Parser[String] = """[-’\w\.,'’]+""".r
  protected def pos: Parser[String] = """[A-Z\$\.,]+""".r
  protected def token: Parser[Token] = word ~ "/" ~ pos ^^ {
    case word ~ "/" ~ pos => Token(word, pos)
  }

  protected def sentence: Parser[Sentence] = wholeNumber ~ rep(token) ^^
    {
      case number ~ list =>
        Sentence(number.toInt, list.toArray)
    }

  protected def cluster: Parser[Cluster] = """@\s""".r ~> """[-\w\s]+\n""".r ~ rep(sentence) <~ "###" ^^
    {
      case name ~ list =>
        //println("Parsed cluster " + name.trim)
        new Cluster(name.trim, list)
    }

  protected def story: Parser[Story] = rep(sentence) <~ "###" ^^
    {
      case list =>
        val array = list.toArray[Sentence]
        for (i <- 0 to array.length - 1) {
          if (i < array.length - 1)
            array(i).next = array(i + 1)
        }

        val story = new Story(array)
        //        /println(story)
        story
    }

  protected def stories: Parser[List[Story]] = rep(story)

  def parseStoryText(text: String): List[Story] =
    {
      val storiesText = text

      val result = parseAll(stories, storiesText)

      result match {
        case Success(x, _) => return x
        case NoSuccess(err, next) => {
          println("failed to parse input " +
            "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
            err + "\n" +
            next.pos.longString)
          System.exit(1)
          Nil
        }
      }
    }

  def parseStories(filename: String): List[Story] =
    {
      val storiesText = scala.io.Source.fromFile(filename).mkString
      //      try {

      val result = parseAll(stories, storiesText)

      result match {
        case Success(x, _) => return x
        case NoSuccess(err, next) => {
          println("failed to parse input " +
            "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
            err + "\n" +
            next.pos.longString)
          System.exit(1)
          Nil
        }
      }
    }

  def parseClusters(filename: String): List[Cluster] =
    {
      val clusterText = scala.io.Source.fromFile(filename).mkString.replaceAll("â€™", "")
      val result = parseAll(rep(cluster), clusterText)

      result match {
        case Success(x, _) => return x
        case NoSuccess(err, next) => {
          println("failed to parse input " +
            "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
            err + "\n" +
            next.pos.longString)
          System.exit(1)
          Nil
        }
      }
    }

}

