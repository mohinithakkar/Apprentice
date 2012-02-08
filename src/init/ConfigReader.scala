package init

import parse._
import java.io._
import java.util.Properties
import scala.collection.mutable.HashMap

class ConfigReader(val configFile: String) {

  val properties = new Properties()
  val in = new FileInputStream(configFile)
  properties.load(in)
  in.close()
  println(properties.getProperty("numRun"))
  println(properties.getProperty("run1"))

  def initData(): (List[Story], List[Cluster]) =
    {
      val storyFile = properties.getProperty("storyFile")
      val clusterFile = properties.getProperty("clusterFile")

      println("using story file: " + storyFile)
      val storyList: List[Story] = GoldParser.parseStories(storyFile)

      println("using cluster file: " + clusterFile)
      val clusterList: List[Cluster] = initClusters(storyList, clusterFile)

      (storyList, clusterList)
    }

  /**
   * initializing the clusters. assigning the sentences to the right story and cluster, so we
   * do not create duplicate sentence objects.
   *
   */
  def initClusters(storyList: List[Story], clusterFile: String) =
    {
      val hashmap = new HashMap[Int, Sentence]
      storyList foreach {
        story =>
          story.members foreach
            {
              sentence =>
                if (hashmap.contains(sentence.id)) throw new ParsingException("sentence repeated" + sentence.id)
                hashmap += ((sentence.id, sentence))
            }
      }

      GoldParser.parseClusters(clusterFile) map {
        c =>
          val newMembers = c.members map
            {
              sentence =>
                // make sure we get the same sentence 
                hashmap.get(sentence.id).get
            }
          val newC = new Cluster(c.name, newMembers)
          newC.members foreach { s =>
            s.cluster = newC
          }
          newC
      }
    }
}

object ConfigReader {
  def main(args: Array[String]) {
    val reader = new ConfigReader("runconfig.txt")

  }
}