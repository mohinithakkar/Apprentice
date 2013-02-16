package edu.gatech.eilab.scheherazade
import main._
import data._
import graph._
import analysis._
import scala.collection.mutable.ListBuffer
import java.io._

package generation {
  
  object StoryGenerator {

    def main(args: Array[String]) {
      val reader = new ConfigReader("configRobBest.txt")
      var (stories, clusters) = reader.initDataFiltered()

      val para = reader.properties.allParameters()(0)

      val minimumSize = para.intParam("minClusterSize")
      val insideClusters = clusters.filterNot(c => c.members.size < minimumSize)
      val insideStories = reader.filterUnused(stories, insideClusters)

      val gen = new GraphGenerator(insideStories, insideClusters)
      var graph: Graph = gen.generate(para)("mutualExcl")._1

      var walk = Walk(graph)
      for (i<- 0 to 10)
    	  randomWalk(walk)
    }

    def randomWalk(firstWalk: Walk) {
      println();println()
      var walk = firstWalk

      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
        val i = math.floor(math.random * fringe.size).toInt
        val step = fringe(i)

        println(step.name)
        walk = walk.forward(step)
      }
    }

    def bruteSearch(firstWalk: Walk) {
      val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("valid stories.txt")))

      var q = scala.collection.mutable.Queue[Walk]()
      var good: Long = 0
      var result = scala.collection.mutable.Set[Walk]()
      q.enqueue(firstWalk)

      while (!q.isEmpty) {
        var n = q.dequeue()

        //println(n)
        //println("fringe: " + n.fringe.map(_.name).mkString("(", ", ", ")"))
        //println("\n\n")    

        //println("story length = " + n.history.size)
        if (!n.hasMoreSteps()) {
          // we have reached the end.
          result += n
        } else {
          n.fringe foreach { step =>
            q += n.forward(step)
          }
        }

        n = null
      }

      //for (story <- result)
      //pw.println("GOOD STORY: \n" + story)

      pw.close()

      println("found " + result.size + " stories.")
      println("Considered " + Walk.totalGenerated + " search nodes. ")
    }
  }
}


//  class Walk(val id: Int, val history: List[Cluster], val fringe: List[Cluster], val exclList: List[Cluster], val selfGraph: Graph) {
//
//    val debug = false
//
//    def nextFringe(step: Cluster, melinks:List[MutualExcl], optionals:List[Cluster]): Walk = {
//      if (!fringe.contains(step)) throw new RuntimeException("Illegal Step: " + step)
//
//      val newHistory = step :: history
//      var excluded = Walk.excluded(List(step), melinks).filter(selfGraph.nodes contains)
//      //println(excluded.map(_.name).mkString("directly mutex: ", ", ", ""))
//      //println(exclList.map(_.name).mkString("old mutex: ", ", ", ""))
//      var excl = excluded ::: exclList
//      excl = findTransitiveClosure(selfGraph, excl)
//      //println(excl.map(_.name).mkString("closure mutex: ", ", ", ""))
//      excluded = excl -- exclList
//      val expired = selfGraph.links.filter(l => l.target == step).map(_.source)
//      val newGraph = selfGraph.addSkipLinks(excluded).removeNodes(excluded ::: expired)
//
//      var newFringe = Walk.maxFringe(newHistory, newGraph, optionals)
//      // delete those already executed
//      newFringe = newFringe filterNot (newHistory contains)
//      
//      new Walk(id, newHistory, newFringe, excl, newGraph)	
//    }
//
//    /**
//     * Takes one step in the graph
//     *
//     */
//    def oneStep(melinks: List[MutualExcl], optionals: List[Cluster]): List[Walk] =
//      {
//        fringe map { step =>
//
//          val newHistory = step :: history
//          var excluded = Walk.excluded(List(step), melinks).filter(selfGraph.nodes contains)
//          //println(excluded.map(_.name).mkString("directly mutex: ", ", ", ""))
//          //println(exclList.map(_.name).mkString("old mutex: ", ", ", ""))
//          var excl = excluded ::: exclList
//          excl = findTransitiveClosure(selfGraph, excl)
//          //println(excl.map(_.name).mkString("closure mutex: ", ", ", ""))
//          excluded = excl -- exclList
//          val expired = selfGraph.links.filter(l => l.target == step).map(_.source)
//          val newGraph = selfGraph.addSkipLinks(excluded).removeNodes(excluded ::: expired)
//
//          var newFringe = Walk.maxFringe(newHistory, newGraph, optionals)
//          // delete those already executed
//          newFringe = newFringe filterNot (newHistory contains)
//          // all steps preceding the step is prohibited
//
//          // enforce an ordering for parallel events
//          val parallel = selfGraph.nodes.filterNot(c => selfGraph.ordered(step, c)).filter(c => c.name > step.name)
//          // newly exclueded by mutual exclusions
//
//          if (debug) {
//            println("*******************************")
//            println(this)
//            println("taking step: " + step.name)
//            println("excluded because of ME: " + excluded.map(_.name).mkString("(", ", ", ")"))
//            println("excluded because of symmetry: " + parallel.map(_.name).mkString("(", ", ", ")"))
//            println("excluded because of temporal ordering: " + expired.map(_.name).mkString("(", ", ", ")"))
//            println("excluded by parent: " + exclList.map(_.name).mkString("(", ", ", ")"))
//            println("is temporal ordering removal necessary: " + (newFringe filter (expired contains)).map(_.name).mkString)
//
//            newGraph.draw("valid")
//
//          }
//
//          newFringe =
//            if ((newFringe -- parallel).isEmpty) newFringe
//            else (newFringe -- parallel)
//
//          val id = Walk.nextId()
//          if (debug) {
//            println("final fringe: " + newFringe.map(_.name).mkString("(", ", ", ")"))
//
//            println("next story : " + id + "\n*******************************")
//            readLine()
//          }
//          new Walk(id, newHistory, newFringe, excl, newGraph)
//
//        }
//      }
//
//    /**
//     * if all direct predecessors of an event is in the event list, add that event to the event list
//     * continue adding events until no such event exists
//     */
//    def findTransitiveClosure(graph: Graph, events: List[Cluster]): List[Cluster] =
//      {
//
//        var all = ListBuffer[Cluster]() ++ events
//        var newFound: ListBuffer[Cluster] = null
//        var remainder = graph.nodes filterNot (all contains)
//        do {
//          newFound = ListBuffer[Cluster]()
//          for (e <- remainder) {
//            val pred = graph.predecessorsOf(e)
//            if ((!pred.isEmpty) &&
//              pred.forall(all contains))
//              newFound += e
//          }
//          all ++= newFound
//          remainder = remainder filterNot (newFound contains)
//        } while (!newFound.isEmpty)
//
//        all.toList
//      }
//
//    def hasMoreSteps() = !fringe.isEmpty
//
//    override def toString(): String = {
//      history.reverse.map(_.name).mkString("Story: " + id + "\n", "\n", "\n***\n")
//    }
//
//    override def equals(o: Any): Boolean = o match {
//      case that: Walk => {
//        if (this.history.size != that.history.length) return false
//        else {
//          val size = this.history.size
//          for (i <- 0 until size) {
//            if (this.history(i) != that.history(i))
//              return false
//          }
//          return true
//        }
//      }
//      case _ => false
//    }
//
//    override def hashCode(): Int = {
//      history.map(_.hashCode).sum * 389 / 311
//    }
//  }
//
//  object Walk {
//
//    var id = 0
//
//    private def nextId() = {
//      id += 1
//      id
//    }
//    private def maxFringe(history: List[Cluster], graph: Graph, optionals: List[Cluster]) =
//      {
//        // if all of its parents are either included in the history or optionals, it is on the fringe
//        val parents = optionals ::: history
//        var possible = graph.nodes.filter { node => graph.predecessorsOf(node).forall(parents.contains) }
//        possible
//      }
//
//    /**
//     * nodes excluded with mutual exclusion links
//     *
//     */
//    private def excluded(history: List[Cluster], melinks: List[MutualExcl]): List[Cluster] =
//      {
//        history.flatMap(s => melinks.filter(m => m.c1 == s).map(m => m.c2) :::
//          melinks.filter(m => m.c2 == s).map(m => m.c1))
//      }
//
//    def fromInits(inits: List[Cluster], graph: Graph, melinks: List[MutualExcl], optionals: List[Cluster]): Walk = {
//      val fringe = inits
//      new Walk(nextId(), Nil, fringe, Nil, graph)
//    }
//  }

