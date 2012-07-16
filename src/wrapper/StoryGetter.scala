package wrapper

import data._
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

object GoldGetter {

  def main(args: Array[String]) {
    val clusterList: List[Cluster] = GoldParser.parseClusters("./data/movie/movieGold2.txt")
    clusterList.foreach {
      cluster =>
        val stringList =
          cluster.members.map {
            sentence =>
              sentence.id + " " + sentence.tokens.map { _.word }.mkString("", " ", "\n")
          }
        println("@ " + cluster.name)
        println(stringList.mkString("", "", "###"))
    }
  }
}