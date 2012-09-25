package edu.gatech.eilab.scheherazade

import data._
import graph._
import analysis._
import scala.collection.mutable.ListBuffer
import java.io._

package main {
  object StoryGenOnDisk {

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
      var graph: Graph = gen.generate(para)("improved")._1
      val eGraph = graph.makeEfficient()

      //    graph.draw("valid")
      //    Thread.sleep(500)

      // generate mutual exclusive links
      var melinks = ListBuffer[MutualExcl]()
      val size = clusters.size
      for (i <- 0 until size; j <- i + 1 until size) {
        val c1 = clusters(i)
        val c2 = clusters(j)

        var count = 0
        for (story <- stories) {
          if (story.members.exists(sent => c1.members.contains(sent)) && story.members.exists(sent => c2.members.contains(sent)))
            count += 1
        }

        val max = math.min(c1.size, c2.size)
        val mi = Cooccurence.mutualInfo(c1, c2, stories)
        if (mi._1 + mi._2 > MI_THRESHOLD && mi._2 > 0)
          melinks += new MutualExcl(c1, c2)
      }

      val me = melinks.toList
      println("me: " + me.mkString("\n"))
      // starting point:
      var sources = eGraph.sourceNodes().map(eGraph.num2Node)
      //println(sources.map(_.name).mkString("sources : ", "\n", ""))
      val ends = eGraph.sinkNodes().map(eGraph.num2Node)
      //println(ends.map(_.name).mkString("ends : ", "\n", ""))
      //readLine()

      // remove from the graph nodes without predecessors that are not sources
      graph = eGraph.removeIrregularSourceEnds()

      val optionals = findOptionals(graph, me)
      graph = graph.addSkipLinks(optionals)
      sources = graph.nodes.filter(n => (!sources.contains(n)) &&
        graph.links.filter(l => l.target == n).map(_.source).forall(optionals contains)) ::: sources

      println(sources.map(_.name).mkString("sources :", "\n", ""))
      println(optionals.map(_.name).mkString("optionals :", "\n", ""))

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
          val string = compactString(n, eGraph)
          //println("produced story " + string)
          val check = known.check(string)
          if (check) {
            pw.println(string)
            good += 1
          }

          val story = decode(string, eGraph)
          println("found **\n" + story)
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

    def decode(code: String, egraph: EfficientGraph): String =
      {
        code map { char:Char =>
          var num = char.toInt
          if (num >= 97) num = num - 97 + 25
          else num -= 65
          
          val node = egraph.num2Node(num)
          node.name
        } mkString("", "\n", "************\n")
        
      }

    def compactString(w: WalkOnDisk, egraph: EfficientGraph): String = {
      w.history.reverse.map { c =>
        var char = ' '
        val num = egraph.node2Num(c)
        if (num <= 25) char = (num + 65).toChar
        else char = (num - 25 + 97).toChar
        char
      }.mkString
    }

    /**
     * optional nodes are nodes that are ordered but mutually exclusive
     *
     */
    def findOptionals(graph: Graph, me: List[MutualExcl]): List[Cluster] =
      {
        // condition 1: c1 and c2 share a mutual exclusion but there is also a path from c1 to c2 on the graph
        val candidates = me.filter(m => graph.ordered(m.c1, m.c2)).map(m => (m.c1, m.c2))
        // condition 2: c1 is not mutually exclusive to another (direct or indirect) predecessor of c2
        val real = candidates.filterNot {
          case (early, late) => me.exists(m =>
            (m.c1 == early && graph.shortestDistance(m.c2, late) != -1) ||
              (m.c2 == early && graph.shortestDistance(m.c1, late) != -1))
        }

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

  object KnownElements {
    /*
  def main(args: Array[String]) {
    val kn = new KnownElements(3)
    println(kn.check("a"))
    println(kn.check("b"))
    println(kn.check("c"))
    println(kn.check("a"))
    println(kn.check("c"))
    //println(kn.check("e"))
    //println(kn.check("f"))
    println(kn.known.mkString)
  } */
  }

}