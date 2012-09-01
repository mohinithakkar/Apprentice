package edu.gatech.eilab.scheherazade
import data._
import graph._
import analysis._
import scala.collection.mutable.ListBuffer
import java.io._
package main {
  object StoryGenerater {

    def main(args: Array[String]) {
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

      println(sources.map(_.name).mkString("sources :", "\n", ""))
      println(optionals.map(_.name).mkString("optionals :", "\n", ""))
      graph.draw("initGraph")

      val firstWalk = Walk.fromInits(sources, graph, me, optionals)
      bruteSearch(firstWalk, ends, me, optionals)
    }

    def bruteSearch(firstWalk: Walk, ends: List[Cluster], me: List[MutualExcl], optionals: List[Cluster]) {
      val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("valid stories.txt")))

      var q = scala.collection.mutable.Queue[Walk]()
      var good: Long = 0
      var result = scala.collection.mutable.Set[Walk]()
      q.enqueue(firstWalk)

      while (!q.isEmpty) {
        var n = q.dequeue()

        //println(n)
        //    println("fringe: " + n.fringe.map(_.name).mkString("(", ", ", ")"))
        //    println("\n\n")    

        //println("story length = " + n.history.size)
        if (ends.exists(c => n.history.contains(c))) {
          // we have reached the end. 
          // end nodes are considered to be mutually exclusive.
          // you can only reach one any time
          //println("GOOD STORY: \n" + n)
          //print(".")
          //good += 1
          //if (good % 1000 == 0) println(good)
          //if (good % 10000 == 0) System.gc()
          result += n
        } else {
          if (n.hasMoreSteps) {
            //print(".")
            //readLine()
            q ++= n.oneStep(me, optionals)
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

      println("found " + result.size + " stories.")
      println("Considered " + Walk.id + " search nodes. ")
    }

    def generateGraph(): (List[Story], List[Cluster], EfficientGraph) =
      {
        val reader = new ConfigReader("configRobBest.txt")
        var (stories, clusters) = reader.initDataFiltered()

        val para = reader.properties.allParameters()(0)

        val minimumSize = para.getParameter("minClusterSize", text => text.trim.toInt).getOrElse(0)
        val insideClusters = clusters.filterNot(c => c.members.size < minimumSize)
        val insideStories = reader.filterUnused(stories, insideClusters)

        val gen = new GraphGenerator(insideStories, insideClusters, para)
        var graph: Graph = gen.generate()._4
        val eGraph = graph.makeEfficient()

        (stories, clusters, eGraph)
      }

    def generateMtlExcl(stories: List[Story], clusters: List[Cluster], threshold: Double) = {
      var melinks = ListBuffer[MutualExcl]()
      val size = clusters.size
      for (i <- 0 until size; j <- i + 1 until size) {
        val c1 = clusters(i)
        val c2 = clusters(j)
        //if (graph.nodes.contains(c1) && graph.nodes.contains(c2) && !graph.ordered(c1, c2) && 
        //c1.name != "Sally sees a gun" && c2.name != "Sally sees a gun") {
        //if (graph.nodes.contains(c1) && graph.nodes.contains(c2)) {
        //only consider parallel c1 and c2
        var count = 0
        for (story <- stories) {
          if (story.members.exists(sent => c1.members.contains(sent)) && story.members.exists(sent => c2.members.contains(sent)))
            count += 1
        }

        val max = math.min(c1.size, c2.size)
        val mi = Cooccurence.mutualInfo(c1, c2, stories)
        if (mi._1 + mi._2 > threshold && mi._2 > 0)
          melinks += new MutualExcl(c1, c2)
      }

      melinks.toList
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

  class Walk(val id: Int, val history: List[Cluster], val fringe: List[Cluster], val exclList: List[Cluster], val selfGraph: Graph) {

    val debug = false

    def nextFringe(step: Cluster, melinks:List[MutualExcl], optionals:List[Cluster]): Walk = {
      if (!fringe.contains(step)) throw new RuntimeException("Illegal Step: " + step)

      val newHistory = step :: history
      var excluded = Walk.excluded(List(step), melinks).filter(selfGraph.nodes contains)
      //println(excluded.map(_.name).mkString("directly mutex: ", ", ", ""))
      //println(exclList.map(_.name).mkString("old mutex: ", ", ", ""))
      var excl = excluded ::: exclList
      excl = findTransitiveClosure(selfGraph, excl)
      //println(excl.map(_.name).mkString("closure mutex: ", ", ", ""))
      excluded = excl -- exclList
      val expired = selfGraph.links.filter(l => l.target == step).map(_.source)
      val newGraph = selfGraph.addSkipLinks(excluded).removeNodes(excluded ::: expired)

      var newFringe = Walk.maxFringe(newHistory, newGraph, optionals)
      // delete those already executed
      newFringe = newFringe filterNot (newHistory contains)
      
      new Walk(id, newHistory, newFringe, excl, newGraph)	
    }

    /**
     * Takes one step in the graph
     *
     */
    def oneStep(melinks: List[MutualExcl], optionals: List[Cluster]): List[Walk] =
      {
        fringe map { step =>

          val newHistory = step :: history
          var excluded = Walk.excluded(List(step), melinks).filter(selfGraph.nodes contains)
          //println(excluded.map(_.name).mkString("directly mutex: ", ", ", ""))
          //println(exclList.map(_.name).mkString("old mutex: ", ", ", ""))
          var excl = excluded ::: exclList
          excl = findTransitiveClosure(selfGraph, excl)
          //println(excl.map(_.name).mkString("closure mutex: ", ", ", ""))
          excluded = excl -- exclList
          val expired = selfGraph.links.filter(l => l.target == step).map(_.source)
          val newGraph = selfGraph.addSkipLinks(excluded).removeNodes(excluded ::: expired)

          var newFringe = Walk.maxFringe(newHistory, newGraph, optionals)
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

          newFringe =
            if ((newFringe -- parallel).isEmpty) newFringe
            else (newFringe -- parallel)

          val id = Walk.nextId()
          if (debug) {
            println("final fringe: " + newFringe.map(_.name).mkString("(", ", ", ")"))

            println("next story : " + id + "\n*******************************")
            readLine()
          }
          new Walk(id, newHistory, newFringe, excl, newGraph)

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
      case that: Walk => {
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

  object Walk {

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

    def fromInits(inits: List[Cluster], graph: Graph, melinks: List[MutualExcl], optionals: List[Cluster]): Walk = {
      val fringe = inits
      new Walk(nextId(), Nil, fringe, Nil, graph)
    }
  }

  class MutualExcl(val c1: Cluster, val c2: Cluster) {
    override def toString() = "ME: " + c1.name + " -/- " + c2.name
  }
}