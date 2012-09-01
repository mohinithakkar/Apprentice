package edu.gatech.eilab.scheherazade

import main.StoryGenerater._
import utils.CSVProcessor
import data._
import graph._

package main {

  object InteractiveEngine {

    def main(args: Array[String]) {
      val desc = readDescriptions("./data/robbery/textDescriptions.csv")

      val MI_THRESHOLD = 0.05
      // generate graph
      val (stories, clusters, eGraph) = generateGraph()
      // generate mutual exclusive links
      val me = generateMtlExcl(stories, clusters, MI_THRESHOLD)
      //    println("me: " + me.mkString("\n"))

      // starting point:
      var sources = eGraph.sourceNodes().map(eGraph.num2Node)
      //println(sources.map(_.name).mkString("sources : ", "\n", ""))
      val ends = eGraph.sinkNodes().map(eGraph.num2Node)
      //println(ends.map(_.name).mkString("ends : ", "\n", ""))
      //readLine()

      // remove from the graph nodes without predecessors that are not sources
      var graph: Graph = eGraph.removeIrregularSourceEnds()

      val optionals = findOptionals(graph, me)
      graph = graph.addSkipLinks(optionals)
      sources = graph.nodes.filter(n => (!sources.contains(n)) &&
        graph.links.filter(l => l.target == n).map(_.source).forall(optionals contains)) ::: sources

      execute(sources, ends, graph, me, optionals, desc)
    }

    def execute(sources: List[Cluster], ends: List[Cluster], graph: Graph, me: List[MutualExcl], optionals: List[Cluster], desc:List[TextDescription]) {
      var walk = Walk.fromInits(sources, graph, me, optionals)
      var step: Cluster = null

      do {
        var fringe = walk.fringe
        var step = makeChoice(fringe, desc)
        walk = walk.nextFringe(step, me, optionals)
      } while (!ends.contains(step))

      println("The End")
      println("Thank you for playing the game")

    }

    def makeChoice(choices: List[Cluster], desc: List[TextDescription]): Cluster = {
      println("Now you have the following choices: ")
      for (i <- 0 until choices.length) {
        val choice = choices(i)
        val optionText = desc.find(text => text.name == choice.name).get.optionText
        println(i + ". " + optionText)
      }

      val n = readLine().trim.toInt

      val chosen = choices(n)
      val result = desc.find(text => text.name == chosen.name).get.SelfExec
      println(result)
      chosen
    }

    def readDescriptions(filename: String): List[TextDescription] = {
      val lines = CSVProcessor.readCSV(filename)
      val answer = for (row <- lines) yield new TextDescription(row(0), row(1), row(2), row(3), row(4))
      answer.toList.tail
    }
  }

  case class TextDescription(val name: String, val actor: String, val optionText: String, val SelfExec: String, val otherExec: String)
}