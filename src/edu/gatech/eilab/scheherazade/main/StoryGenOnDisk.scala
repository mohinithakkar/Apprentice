package edu.gatech.eilab.scheherazade

import data._
import graph._
import analysis._
import scala.collection.mutable.ListBuffer
import java.io._

package main {
  object StoryGenOnDisk {

    var node2Num: Map[Cluster, Int] = null
    var num2Node: Map[Int, Cluster] = null

    def main(args: Array[String]) {

      val MI_THRESHOLD = 0.05

      // generate graph
      val reader = new ConfigReader("configRobBest.txt")
      var (stories, clusters) = reader.initDataFiltered()

      val para = reader.properties.allParameters()(0)

      val minimumSize = para.intParam("minClusterSize")
      val insideClusters = clusters.filterNot(c => c.members.size < minimumSize)
      val insideStories = reader.filterUnused(stories, insideClusters)

      val gen = new GraphGenerator(insideStories, insideClusters)
      var graph: Graph = gen.generate(para)("mutualExcl")._1
      graph.draw("ondisk")
      Thread.sleep(1000)

      val me = graph.mutualExcls
      println("me: " + me.mkString("\n"))
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

      println(sources.map(_.name).mkString("sources :", "\n", ""))
      println(optionals.map(_.name).mkString("optionals :", "\n", ""))
      println("******************************************")
      randomSelectStories()
      System.exit(1)

      val firstWalk = WalkOnDisk.fromInits(sources, graph, me, optionals)

      val dir = new File("./stats")
      if (!dir.exists()) dir.mkdir()

      val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("./stats/valid stories.txt")))

      var q = scala.collection.mutable.Stack[WalkOnDisk]()
      var good: Long = 0
      val known = new KnownElements(10000)

      q.push(firstWalk)

      while (!q.isEmpty) {
        var n = q.pop()

        //println(n)
        //    println("fringe: " + n.fringe.map(_.name).mkString("(", ", ", ")"))
        //    println("\n\n")    

        //println("story length = " + n.history.size)
        if (ends.exists(c => n.history.contains(c))) {
          // we have reached the end. 
          // end nodes are considered to be mutually exclusive.
          // you can only reach one any time
          val string = compactString(n)
          //println("produced story " + string)
          val check = known.check(string)
          if (check) {
            pw.println(string)
            good += 1
          }

          //          val story = decode(string)
          //          println("found **\n" + story)
        } else {
          if (n.hasMoreSteps) {
            //print(".")
            //readLine()
            q ++= (n.oneStep(me, optionals))
          } else {
            println("WARNING: CANNOT REACH AN ENDING. \n" + n)
            //println("WARNING: CANNOT REACH AN ENDING. \n" + n)
          }
        }

        n = null
      }

      //for (story <- result)
      //pw.println("GOOD STORY: \n" + story)

      pw.close()

      println("found " + good + " stories.")
      println("Considered " + WalkOnDisk.id + " search nodes. ")
    }

    def decode(code: String): String =
      {
        code map { char: Char =>
          var num = char.toInt
          if (num >= 97) num = num - 97 + 25
          else num -= 65

          val node = num2Node(num)
          node.name
        } mkString ("", "\n", "************\n")

      }

    def randomSelectStories() {
      //val hexText = scala.io.Source.fromFile("./stats/valid stories.txt")
      //val list = hexText.mkString.split("\n")
      val length = 14000000
      println("length = " + length)
      val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("random_stories.txt")))
      
      for (i <- 1 to 400) {
        println(i)
        val idx = math.rint(math.random * length).toInt
        val hex = readLineFromLargeFile("./stats/valid stories.txt", idx);
        for (j <- 0 until hex.length) {
          val char = hex.charAt(j)
          var num = char.toInt
          if (num >= 97) num = num - 97 + 25
          else num -= 65

          val node = num2Node(num)
          pw.print("CS" + i + "\t@ " + node.name)
          if (j == 0) pw.println("\t\t" + hex)
          else pw.println
        }
      }
      
      pw.close()
    }
    
    
    
    def readLineFromLargeFile(filename:String, lineNo:Int):String = 
    {
      val reader = new BufferedReader(new FileReader(filename))
      for (i <- 0 until lineNo)
      {
        reader.readLine()
      }
      reader.readLine()
    }
    

    def printMappings() {
      for (pairs <- node2Num) {
        val node = pairs._1
        val num = pairs._2
        print(node.name + ", ")
        var char = ' '
        if (num <= 25) char = (num + 65).toChar
        else char = (num - 25 + 97).toChar
        println(char)
      }
    }

    def compactString(w: WalkOnDisk): String = {
      w.history.reverse.map { c =>
        var char = ' '
        val num = node2Num(c)
        if (num <= 25) char = (num + 65).toChar
        else char = (num - 25 + 97).toChar
        char
      }.mkString
    }

    /**
     * optional nodes are nodes that are ordered but mutually exclusive
     *
     */
    def findOptionals(graph: Graph): List[Cluster] =
      {
        // condition 1: c1 and c2 share a mutual exclusion but there is also a path from c1 to c2 on the graph
        val candidates = graph.mutualExcls.filter(m => graph.ordered(m.c1, m.c2)).map(m => (m.c1, m.c2))
        println("candidates:\n" + candidates.mkString("\n"))
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

              println(prevent.mkString(" ") + " prevents " + early.name + " " + late.name);
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

  }

  class WalkOnDisk(val id: Int, val history: List[Cluster], val fringe: List[Cluster], val exclList: List[Cluster], val selfGraph: Graph) {

    val debug = false

    /**
     * Takes one step in the graph
     *
     */

    def oneStep(melinks: List[MutualExcl], optionals: List[Cluster]): List[WalkOnDisk] =
      {
        fringe map { step =>

          val newHistory = step :: history
          var excluded = WalkOnDisk.excluded(List(step), melinks).filter(selfGraph.nodes contains)
          //println(excluded.map(_.name).mkString("directly mutex: ", ", ", ""))
          //println(exclList.map(_.name).mkString("old mutex: ", ", ", ""))
          var excl = excluded ::: exclList
          excl = findTransitiveClosure(selfGraph, excl)
          //println(excl.map(_.name).mkString("closure mutex: ", ", ", ""))
          excluded = excl -- exclList
          val expired = selfGraph.links.filter(l => l.target == step).map(_.source)
          val newGraph = selfGraph.addSkipLinks(excluded).removeNodes(excluded ::: expired)

          var newFringe = WalkOnDisk.maxFringe(newHistory, newGraph, optionals)
          // delete those already executed
          newFringe = newFringe filterNot (newHistory contains)
          // all steps preceding the step is prohibited

          // enforce an ordering for parallel events
          val parallel = selfGraph.nodes.filterNot(c => selfGraph.ordered(step, c)).filter(c => c.name > step.name)
          // newly exclueded by mutual exclusions

          if (debug) {
            println("*******************************")
            println(this)
            println("taking step: " + step.name)
            println("excluded because of ME: " + excluded.map(_.name).mkString("(", ", ", ")"))
            println("excluded because of symmetry: " + parallel.map(_.name).mkString("(", ", ", ")"))
            println("excluded because of temporal ordering: " + expired.map(_.name).mkString("(", ", ", ")"))
            println("excluded by parent: " + exclList.map(_.name).mkString("(", ", ", ")"))
            println("is temporal ordering removal necessary: " + (newFringe filter (expired contains)).map(_.name).mkString)

            newGraph.draw("valid")

          }

          /*
        // exclude symmetry
        newFringe =
          if ((newFringe -- parallel).isEmpty) newFringe
          else (newFringe -- parallel)
		*/

          val id = WalkOnDisk.nextId()
          if (debug) {
            println("final fringe: " + newFringe.map(_.name).mkString("(", ", ", ")"))

            println("next story : " + id + "\n*******************************")
            readLine()
          }
          new WalkOnDisk(id, newHistory, newFringe, excl, newGraph)

        }
      }

    /**
     * if all direct predecessors of an event is in the event list, add that event to the event list
     * continue adding events until no such event exists
     */
    def findTransitiveClosure(graph: Graph, events: List[Cluster]): List[Cluster] =
      {

        var all = ListBuffer[Cluster]() ++ events
        var newFound: ListBuffer[Cluster] = null
        var remainder = graph.nodes filterNot (all contains)
        do {
          newFound = ListBuffer[Cluster]()
          for (e <- remainder) {
            val pred = graph.predecessorsOf(e)
            if ((!pred.isEmpty) &&
              pred.forall(all contains))
              newFound += e
          }
          all ++= newFound
          remainder = remainder filterNot (newFound contains)
        } while (!newFound.isEmpty)

        all.toList
      }

    def hasMoreSteps() = !fringe.isEmpty

    override def toString(): String = {
      history.reverse.map(_.name).mkString("Story: " + id + "\n", "\n", "\n***\n")
    }

    override def equals(o: Any): Boolean = o match {
      case that: WalkOnDisk => {
        if (this.history.size != that.history.length) return false
        else {
          val size = this.history.size
          for (i <- 0 until size) {
            if (this.history(i) != that.history(i))
              return false
          }
          return true
        }
      }
      case _ => false
    }

    override def hashCode(): Int = {
      history.map(_.hashCode).sum * 389 / 311
    }

  }

  object WalkOnDisk {

    var id = 0

    private def nextId() = {
      id += 1
      id
    }
    private def maxFringe(history: List[Cluster], graph: Graph, optionals: List[Cluster]) =
      {
        // if all of its parents are either included in the history or optionals, it is on the fringe
        val parents = optionals ::: history
        var possible = graph.nodes.filter { node => graph.predecessorsOf(node).forall(parents.contains) }
        possible
      }

    /**
     * nodes excluded with mutual exclusion links
     *
     */
    private def excluded(history: List[Cluster], melinks: List[MutualExcl]): List[Cluster] =
      {
        history.flatMap(s => melinks.filter(m => m.c1 == s).map(m => m.c2) :::
          melinks.filter(m => m.c2 == s).map(m => m.c1))
      }

    def fromInits(inits: List[Cluster], graph: Graph, melinks: List[MutualExcl], optionals: List[Cluster]): WalkOnDisk = {
      val fringe = inits
      new WalkOnDisk(nextId(), Nil, fringe, Nil, graph)
    }
  }

  /**
   * A circular array serving as a buffer
   *
   */
  class KnownElements(val size: Int) {

    var head = 0 // head is the position where comparison begins
    var tail = 0 // tail is the position where insertion happens
    var full = false
    // circular array
    var known = new Array[String](size)

    def check(str: String): Boolean = {
      //val str = walk.toCompactString()
      var index = head

      while (index != tail) {
        //println("compariing with " + known(index))
        if (known(index) == str)
          return false
        else index = (index + 1) % size
      }

      if (full) {
        // compare with tail, too
        //println("comparing with " + known(tail))
        if (known(tail) == str) return false
      }

      // every thing is compared. replace the head
      known(tail) = str
      tail = (tail + 1) % size
      if (!full) {
        if (tail == 0) {
          full = true
          head = 1
        }
      } else {
        head = (tail + 1) % size
      }
      true
    }
  }

}