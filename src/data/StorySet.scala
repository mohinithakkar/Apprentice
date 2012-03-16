package data

import parse._
import java.io._
class StorySet(
  val name: String,
  var storyList: List[Story]) {
}

object PersistenceTest extends App {

  val storyFile = "./data/movie/movieStories.txt"
  var storyList: List[Story] = GoldParser.parseStories(storyFile)
  val xml = XStream.toXML(storyList)
  println(xml)
  
  val pw = new PrintWriter(new FileOutputStream("test.xml"))
  pw.println(xml)
  pw.close()
}