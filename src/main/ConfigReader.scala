package main

import parse._
import java.io._
import java.util.Properties
import scala.collection.mutable.HashMap

class ConfigReader(val configFile: String) {

  val properties = new Properties()
  val in = new FileInputStream(configFile)
  properties.load(in)
  in.close()
  println(properties.getProperty("parameters"))
  
  def allParameters():Array[Properties] =
  {
    var params:Array[Properties] = new Array[Properties](0)
    val paraNames = properties.getProperty("parameters").split(",")
    paraNames foreach { n =>
      val name = n.trim
      // for each parameter listed, we get a list of values
      val values = properties.getProperty(name).split(",")
      if (params.isEmpty)
      {
        params = values.map{v => 
          val p = new Properties()
          p.setProperty(name, v)
          p}
      }
      else
      {
        // for each existing Properties object, we append this parameter
        params = params flatMap {param =>
        	values.map{v =>
        	  val p = param.clone().asInstanceOf[Properties]
        	  p.setProperty(name, v)
        	  p
        	}
        }
      }
    }
    
    params
  }

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
    val (stories, clusters) = reader.initData()
    val parameters = reader.allParameters()
    parameters foreach { para =>
//      println(para)
//      println(para.getProperty("confThresholds"))
//      println(para.containsKey("confThresholds"))
      val gen = new GraphGenerator(stories, clusters, para)
      gen.generate()
    }
  }
}