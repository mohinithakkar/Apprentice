package pipeline

import data._
import parse._
import scala.collection.mutable.HashMap

class StoryParserPline extends Pipeline[String, List[Story]] {

  private var storyFile: String = "unspecified"
  def apply(filename: String): List[Story] = {
    storyFile = filename.replaceAll(".txt", "")
    GoldParser.parseStories(filename)
  }

  val name = storyFile
}

class ClusterParserPline(val clusterFile: String) extends Pipeline[List[Story], List[Cluster]] {

  def apply(storylist: List[Story]): List[Cluster] = {
    initClusters(storylist, clusterFile)
  }

  val name = clusterFile

  /** initializing the clusters. assigning the sentences to the right story and cluster, so we
   *  do not create duplicate sentence objects.
   *
   */
  def initClusters(storyList: List[Story], clusterFile: String) =
    {
      val hashmap = new HashMap[Int, Sentence]
      storyList foreach {
        story =>
          story.members foreach
            { sentence =>
              if (hashmap.contains(sentence.id)) throw new ParsingException("sentence repeated" + sentence.id)
              hashmap += ((sentence.id, sentence))
            }
      }

      GoldParser.parseClusters(clusterFile) map {
        c =>
          val newMembers = c.members map
            { sentence =>
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