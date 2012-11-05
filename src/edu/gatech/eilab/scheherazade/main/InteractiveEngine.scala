package edu.gatech.eilab.scheherazade

import main.StoryGenerater._
import utils.CSVProcessor
import data._
import graph._

import java.io._

package main {

  //TODO: A thorough test to make sure there is bugs and is consistent with the graph. 
  //TODO: Prompts for the time the engine waits for user input.
  object InteractiveEngine {

    var node2Num: Map[Cluster, Int] = null
    var num2Node: Map[Int, Cluster] = null

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

      val me = graph.mutualExcls
      
      // starting point:
      var sources = graph.nodes.filterNot(n => graph.links.exists(l => l.target == n))
      //println(sources.map(_.name).mkString("sources : ", "\n", ""))
      val ends = graph.nodes.filterNot(n => graph.links.exists(l => l.source == n))
      //println(ends.map(_.name).mkString("ends : ", "\n", ""))
      //readLine()

      node2Num = {
        val sorted = graph.nodes.sortWith((x, y) => x.name > y.name) // introduce an arbitrary ordering between the clusters
        val num = 0 until graph.nodes.length
        (sorted zip num).toMap
      }

      num2Node = {
        node2Num.map { case (x, y) => (y, x) }
      }

      // remove from the graph nodes without predecessors that are not sources
      //graph = eGraph.removeIrregularSourceEnds()

      val optionals = findOptionals(graph)
      graph = graph.addSkipLinks(optionals)
      sources = graph.nodes.filter(n => (!sources.contains(n)) &&
        graph.links.filter(l => l.target == n).map(_.source).forall(optionals contains)) ::: sources

      execute(sources, ends, graph, me, optionals, desc)
    }

    def findOptionals(graph: Graph): List[Cluster] =
      {
        // condition 1: c1 and c2 share a mutual exclusion but there is also a path from c1 to c2 on the graph
        val candidates = graph.mutualExcls.filter(m => graph.ordered(m.c1, m.c2)).map(m => (m.c1, m.c2))
        //println("candidates:\n" + candidates.mkString("\n"))
        // condition 2: c1 is not mutually exclusive to another (direct or indirect) predecessor of c2
        val real = candidates.filterNot {
          case (c1, c2) =>
            var early: Cluster = null
            var late: Cluster = null
            if (graph.shortestDistance(c1, c2) != -1) {
              early = c1
              late = c2
            } else {
              early = c2
              late = c1
            }

            val bool = graph.mutualExcls.exists(m =>
              (m.c1 == early && m.c2 != late && graph.shortestDistance(m.c2, late) != -1) ||
                (m.c2 == early && m.c1 != late && graph.shortestDistance(m.c1, late) != -1))

            if (bool) {
              val prevent = graph.mutualExcls.filter(m =>
                (m.c1 == early && graph.shortestDistance(m.c2, late) != -1) ||
                  (m.c2 == early && graph.shortestDistance(m.c1, late) != -1))

              //println(prevent.mkString(" ") + " prevents " + early.name + " " + late.name);
            }
            bool
        }

        /*
        candidates foreach {
          case (early, late) => 
            graph.mutualExcls.foreach(m =>
              if ((m.c1 == early && graph.shortestDistance(m.c2, late) != -1) ||
              (m.c2 == early && graph.shortestDistance(m.c1, late) != -1))
              
            println(m + " prevents " + early.name + " " + late.name);
        }*/

        real.flatMap(x => List(x._1, x._2))
      }

    def execute(sources: List[Cluster], ends: List[Cluster], graph: Graph, me: List[MutualExcl], optionals: List[Cluster], desc: List[TextDescription]) {
      var playAgain = true
      do {
        var walk = Walk.fromInits(sources, graph, me, optionals)
        var step: Cluster = null

        val actors = desc.map(_.actor).distinct

        var actor = "John"
//        while (actor == "") {
//          println("Choose an actor: ")
//          for (i <- 0 until actors.length) {
//            println((i + 1) + ". " + actors(i))
//          }
//
//          val actorNum = readLine().toInt - 1
//
//          if (actorNum < 0 || actorNum >= actors.length) println("Invalid choice")
//          else actor = actors(actorNum)
//        }

        do {
          var fringe = walk.fringe
          step = makeChoice(fringe, desc, actor)
          walk = walk.nextFringe(step, me, optionals)
        } while (!ends.contains(step))

        println("The End.\n\n\nLet's play again!\n\n")
        var input: Char = 0
//        while (input != 'Y' && input != 'y' && input != 'N' && input != 'n') {
//          print("Play again (Y or N)?")
//          val line = readLine().trim
//          if (line.length > 0)
//            input = line.charAt(0)
//        }

//        if (input == 'Y' || input == 'y')
//          playAgain = true
//        else playAgain = false

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
                readText = new String(char).substring(0, 1)
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
  /*
  object ReadMaybe {
    
    import java.io.FileInputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedByInterruptException;

	final buf = ByteBuffer.allocate(4096);

	def readLine(timeout:Long):String = {
			val in = extract(System.in);
			if (!(in instanceof FileInputStream))
				throw new RuntimeException(
						"Could not extract a FileInputStream from STDIN.");

				val ret = maybeAvailable(in.asInstanceOf[FileInputStream], timeout);
				if (ret == 0)
					return "";
				else {
					val result = buf.asCharBuffer().subSequence(0, ret).toString();
					return result;
				}
	}

	/*
	 * unravels all layers of FilterInputStream wrappers to get to the core
	 * InputStream
	 */
	private def extract(in:InputStream): InputStream = {

	  var k = in
		val f = classOf[FilterInputStream].getDeclaredField("in");
		f.setAccessible(true);

		while (k.isInstanceof[FilterInputStream])
			k = f.get(k.asInstanceOf[FilterInputStream]).asInstanceOf[InputStream]

		return in
	}

	/*
	 * Returns the number of bytes which could be read from the stream, timing
	 * out after the specified number of milliseconds. Returns 0 on timeout
	 * (because no bytes could be read) and -1 for end of stream.
	 */
	private def maybeAvailable(in:FileInputStream, timeout:Long):Int =
			 {

		var dataReady = Array[Int].ofDim(1);
		final IOException[] maybeException = { null };
		final Thread reader = new Thread() {
			public void run() {
				try {
					dataReady[0] = in.getChannel().read(buf);
				} catch (ClosedByInterruptException e) {
					System.err.println("Reader interrupted.");
				} catch (IOException e) {
					maybeException[0] = e;
				}
			}
		};

		Thread interruptor = new Thread() {
			public void run() {
				reader.interrupt();
			}
		};

		reader.start();
		for (;;) {

			reader.join(timeout);
			if (!reader.isAlive())
				break;

			interruptor.start();
			interruptor.join(1000);
			reader.join(1000);
			if (!reader.isAlive())
				break;

			System.err.println("Fatal: Timeout during readline is not supported. Are you running this with Eclipse within an IDE? ");
			System.exit(1);
		}

		if (maybeException[0] != null)
			throw maybeException[0];

		return dataReady[0];
	}
}
*/
}