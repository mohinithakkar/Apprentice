package wrapper

import parse._
import data._

object StoryGetter {

  def main(args: Array[String]) {
    val storyList: List[Story] = GoldParser.parseStories("restaurantStories.txt")
    storyList.foreach {
      story =>
        val stringList =
          story.members.map {
            sentence =>
              sentence.tokens.map { _.word }.mkString("", " ", "\n")
          }
        println(stringList.mkString("", "", "###"))
    }
  }
}