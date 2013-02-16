package edu.gatech.eilab.scheherazade

import main._
import utils.CSVProcessor
import data._
import graph._

import java.io._

package generation {

  //TODO: A thorough test to make sure there is bugs and is consistent with the graph. 
  //TODO: Prompts for the time the engine waits for user input.
  object InteractiveEngine {

    var graphIndex:GraphIndex = null

    def main(args: Array[String]) {
      val desc = readDescriptions("./data/robbery/textDescriptions.csv")

      val reader = new ConfigReader("configRobBest.txt")
      var (stories, clusters) = reader.initDataFiltered()

      val para = reader.properties.allParameters()(0)

      val minimumSize = para.intParam("minClusterSize")
      val insideClusters = clusters.filterNot(c => c.members.size < minimumSize)
      val insideStories = reader.filterUnused(stories, insideClusters)

      val gen = new GraphGenerator(insideStories, insideClusters)
      var graph: Graph = gen.generate(para)("mutualExcl")._1

      graph.draw("abcdefg.png")
      var walk = Walk(graph)


      execute(walk, desc)
    }

    def execute(walk:Walk, desc: List[TextDescription]) {
      var playAgain = true
      do {
        var stroll = walk
        var step: Cluster = null
        val actors = desc.map(_.actor).distinct
        var actor = "John"
        /* the following allows you to select an actor other than John. Now only John is allowed
           * 
            while (actor == "") {
              println("Choose an actor: ")
              for (i <- 0 until actors.length) {
                println((i + 1) + ". " + actors(i))
              }
    
              val actorNum = readLine().toInt - 1
    
              if (actorNum < 0 || actorNum >= actors.length) println("Invalid choice")
              else actor = actors(actorNum)
            }
           */
        do {
          var fringe = stroll.fringe
          step = makeChoice(fringe, desc, actor)
          stroll = stroll.forward(step)
        } while (stroll.hasMoreSteps)

        println("The End.\n\n\nLet's play again!\n\n")
        var input: Char = 0

        /* this code is prompt for if you want to play again. Disabled for now
         * 
        while (input != 'Y' && input != 'y' && input != 'N' && input != 'n') {
          print("Play again (Y or N)?")
          val line = readLine().trim
          if (line.length > 0)
            input = line.charAt(0)
        }

        if (input == 'Y' || input == 'y')
          playAgain = true
        else playAgain = false
         */

      } while (playAgain)
      println("Thank you for playing the game! \n Copyright 2012 Entertainment Intelligence Lab, Georgia Tech.")
    }

    def makeChoice(choices: List[Cluster], desc: List[TextDescription], actor: String): Cluster = {

      val descripts = choices.map { c =>
        val o = desc.find(_.name == c.name)
        if (o.isEmpty) throw new RuntimeException(c.name + " is missing from the descriptions")
        val d = o.get
        (c, d)
      }

      val humanChoices = descripts.filter(pair => pair._2.actor == actor)
      val npcChoices = descripts filterNot (humanChoices contains)
      var chosen: (Cluster, TextDescription) = null
      var madeByHuman = true

      while (chosen == null) { // repeat until a choice is made

        var readText = ""

        if (humanChoices.size > 0) {
          println("Now you have the following choices: ")
          for (i <- 0 until humanChoices.length) {
            val optionText = humanChoices(i)._2.optionText
            println((i + 1) + ". " + optionText)
          }
          print("Your choice is: ")

          if (npcChoices.size > 0) {

            // can make a npc choice. apply a timeout on input
            var sleep = 0
            while (readText == "" && sleep < 10) {
              if (System.in.available() > 0) {
                val char = Array.ofDim[Char](3)
                var i = 0
                while (System.in.available() > 0 && i < 3) {
                  char(i) = System.in.read().asInstanceOf[Char]
                  i += 1
                }
                readText = new String(char).trim
                // guarding against empty input
                if (readText.length > 0)
                  readText = readText.substring(0, 1)

              } else {
                sleep += 1
                Thread.sleep(500) // sleep for 0.5 sec
              }
            }

            if (readText == "") println() // this line break comes after "Your choice is:"

          } else {
            readText = readLine()
          }
        }

        try {
          if (readText == "") {

            if (npcChoices.length > 0) {
              // select a npc choice            
              val idx = math.floor(math.random * npcChoices.length).toInt
              chosen = npcChoices(idx)
              madeByHuman = false
            }
          } else {

            if (humanChoices.size == 0) throw new RuntimeException("No choices left. That's weird.")

            val idx = readText.toInt - 1
            if (idx < 0 || idx >= humanChoices.length)
              println("Invalid Choice. Choose from 1 to " + humanChoices.length)
            else {
              chosen = humanChoices(idx)
            }
          }
        } catch {
          // catch the number format exception that may happen when converting a string to a number
          // we do not need to do anything there because the choice is just empty if there is an exception
          case numEx: NumberFormatException =>
        }
      }

      val result = if (madeByHuman) chosen._2.SelfExec else chosen._2.otherExec
      if (result == "None") println("problematic: " + chosen._2)
      println()
      println(result)
      println()
      chosen._1
    }

    def readDescriptions(filename: String): List[TextDescription] = {
      val lines = CSVProcessor.readCSV(filename)
      val answer = for (row <- lines) yield new TextDescription(row(0), row(1), row(2), row(3), row(4))
      answer.toList.tail
    }
  }

  case class TextDescription(val name: String, val actor: String, val optionText: String, val SelfExec: String, val otherExec: String)

}