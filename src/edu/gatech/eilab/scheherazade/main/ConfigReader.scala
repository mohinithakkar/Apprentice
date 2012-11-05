package edu.gatech.eilab.scheherazade

import io._
import parse._
import data._
import xml._
import javanlp._
import graph._
import utils.SuperProperties
import java.io._
import java.util.Properties
import scala.collection.mutable.{HashMap, ListBuffer, Queue}

package main {
  class ConfigReader(val configFile: String) {

    val properties = new SuperProperties()
    val in = new FileInputStream(configFile)
    properties.load(in)
    in.close()
    println("Parameters supplied: " + properties.getProperty("parameters"))


    def initData(): (List[Story], List[Cluster]) =
      {
        val storyFile = properties.getProperty("storyFile")
        val clusterFile = properties.getProperty("clusterFile")

        //println("using story file: " + storyFile)
        var storyList: List[Story] = SimpleParser.parseStories(storyFile)
        //GoldParser.parseStories(storyFile)

        storyList.foreach(_.addStoryLocation())

        println("using cluster file: " + clusterFile)
        val clusterList: List[Cluster] = initClusters(storyList, clusterFile)

        // This filters unused sentences in the stories
        //storyList = filterUnused(storyList, clusterList)
        (storyList, clusterList)
      }

    def initDataFiltered(): (List[Story], List[Cluster]) =
      {
        val storyFile = properties.getProperty("storyFile")
        val clusterFile = properties.getProperty("clusterFile")

        println("using story file: " + storyFile)
        var storyList: List[Story] = SimpleParser.parseStories(storyFile)
        //GoldParser.parseStories(storyFile)
        //println(storyList.map(_.members.map(_.toShortString()).mkString("\n")).mkString("\n###\n"))
        //System.exit(1)
        storyList.foreach(_.addStoryLocation())

        println("using cluster file: " + clusterFile)
        val clusterList: List[Cluster] = initClusters(storyList, clusterFile)

        // This filters unused sentences in the stories
        storyList = filterUnused(storyList, clusterList)
        (storyList, clusterList)
      }

    def filterUnused(storyList: List[Story], clusterList: List[Cluster]): List[Story] =
      {
        val used = clusterList.flatMap { _.members }
        storyList map { story =>
          val newMembers = story.members.filter { s => used.contains(s) }
          //val str = story.members.filterNot { s => used.contains(s) }.map { _.toShortString() }.mkString("\n")
          //println(str)
          new Story(newMembers)
        }
      }

    /**
     * initializing the clusters. assigning the sentences to the right story and cluster, so we
     *  do not create duplicate sentence objects.
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
                //println(sentence.id)
              }
        }

        SimpleParser.parseClusters(clusterFile) map {
          c =>
            //println("name = " + c.name)
            val newMembers = c.members map
              {
                sentence =>
                  // make sure we get the same sentence 
                  val ans = hashmap.get(sentence.id)
                  if (ans.isEmpty) throw new Exception("Did not find the same sentence in the story file: " + sentence.id)

                  ans.get
              }
            val newC = new Cluster(c.name, newMembers)
            newC.members foreach { s =>
              s.cluster = newC
            }
            newC
        }

      }

    /**
     * print the sentence that does not end with a period
     *
     */
    def findIrregularSent(stories: List[Story]) {
      val temp = stories.flatMap(_.members).filterNot(s => s.tokens.last.word.trim.endsWith(".")).mkString
      println(temp)
      System.exit(0)
    }

  }

  
}