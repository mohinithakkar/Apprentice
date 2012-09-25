package edu.gatech.eilab.scheherazade

import main.StoryGenerater._
import utils.CSVProcessor
import data._
import graph._

import java.io._
import java.nio.channels.ClosedChannelException

package main {

  //TODO: A thorough test to make sure there is bugs and is consistent with the graph. 
  object InteractiveEngine {

    def main(args: Array[String]) {
      val desc = readDescriptions("./data/robbery/textDescriptions.csv")

      val MI_THRESHOLD = 0.05
      // generate graph
      val (stories, clusters, eGraph) = generateGraph()
      // generate mutual exclusive links
      val me = generateMtlExcl(stories, clusters, MI_THRESHOLD)
      println("me: " + me.mkString("\n"))

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
      
      graph.draw("iegraph")
      
      execute(sources, ends, graph, me, optionals, desc)
    }

    def execute(sources: List[Cluster], ends: List[Cluster], graph: Graph, me: List[MutualExcl], optionals: List[Cluster], desc: List[TextDescription]) {
      var playAgain = true
      
      do {
        val graphCopy = graph.clone()
        var walk = Walk.fromInits(sources, graphCopy, me, optionals)
        var step: Cluster = null

        val actors = desc.map(_.actor).distinct

        var actor = ""
        while (actor == "") {
          println("Choose an actor: ")
          for (i <- 0 until actors.length) {
            println((i + 1) + ". " + actors(i))
          }

          val actorNum = readLine().toInt - 1

          if (actorNum < 0 || actorNum >= actors.length) println("Invalid choice")
          else actor = actors(actorNum)
        }

        do {
          var fringe = walk.fringe
          println("fringe = " + fringe.map(_.name).mkString(", "))
          step = makeChoice(fringe, desc, actor)
          walk = walk.nextFringe(step, me, optionals)
          
        } while (!ends.contains(step))

        println("The End.")
        var input: Char = 0
        while (input != 'Y' && input != 'y' && input != 'N' && input != 'n') {
          print("Play again (Y or N)?")
          val line = readLine().trim
          if (line.length > 0)
            input = line.charAt(0)
        }

        if (input == 'Y' || input == 'y')
          playAgain = true
        else playAgain = false

      } while (playAgain)
      println("Thank you for playing the game! \nCopyright @ 2012 Entertainment Intelligence Lab, Georgia Tech.")
    }

    def makeChoice(choices: List[Cluster], desc: List[TextDescription], actor: String): Cluster = {

      val descripts = choices.map { c =>
        val d = desc.find(_.name == c.name).get
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