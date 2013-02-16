package edu.gatech.eilab.scheherazade

import data._
import main._
import graph._
import scala.collection.mutable.ListBuffer

package generation {

  class Walk(val id: Int, val graph: Graph, val sources: List[Cluster], val ends: List[Cluster], val mutex: List[MutualExcl],
    val optional: List[Cluster], val history: List[Cluster], val fringe: List[Cluster], val excluded: List[Cluster]) {

    def this(graph: Graph, sources: List[Cluster], ends: List[Cluster], mutex: List[MutualExcl], optional: List[Cluster], fringe: List[Cluster]) =
      this(0, graph, sources, ends, mutex, optional, List[Cluster](), fringe, List[Cluster]())

    /**
     *  Takes one step forward in the graph
     *
     *
     */
    def forward(next: Cluster): Walk =
      {
        val step = next
        if (!(fringe contains step)) throw new RuntimeException("Illegal step: " + step.name)

        // add the step to history. Note history is in reverse order
        val newHistory = step :: history

        // direct mutex
        var newlyExcluded = computeExcluded(List(step), mutex).filter(graph.nodes contains)

        var allExcluded = newlyExcluded ::: excluded

        // recursive mutex
        allExcluded = findTransitiveClosure(graph, allExcluded)

        // update newly excluded. These are used later
        newlyExcluded = allExcluded -- excluded

        // nodes cannot execute any more due to temporal constraints
        val expired = graph.links.filter(l => l.target == step).map(_.source)

        // first add links for skipped nodes and then remove these nodes
        val newGraph = graph.addSkipLinks(newlyExcluded).removeNodes(newlyExcluded ::: expired)

        var newFringe = maxFringe(newGraph, newHistory)
        // delete those already executed
        newFringe = newFringe filterNot (newHistory contains)
        // all steps preceding the step is prohibited

        // the following line enforces an ordering for parallel events, but we now do not need it
        // val parallel = graph.nodes.filterNot(c => graph.ordered(step, c)).filter(c => c.name > step.name)

        if (Walk.debug) {
          println("*******************************")
          println("Walk " + id)
          println("taking step: " + step.name)
          println("excluded because of ME: " + newlyExcluded.map(_.name).mkString("(", ", ", ")"))
          //println("excluded because of symmetry: " + parallel.map(_.name).mkString("(", ", ", ")"))
          println("excluded because of temporal ordering: " + expired.map(_.name).mkString("(", ", ", ")"))
          println("excluded by parent: " + allExcluded.map(_.name).mkString("(", ", ", ")"))          

          newGraph.draw("valid")

          println("final fringe: " + newFringe.map(_.name).mkString("(", ", ", ")"))

          println("next story : " + id + "\n*******************************")
          readLine()
        }

        new Walk(Walk.nextId(), newGraph, sources, ends, mutex, optional, newHistory, newFringe, allExcluded)
      }

    /**
     * Can we walk more? Not if the history already contains one story ending
     *
     */
    def hasMoreSteps() = !(ends exists { history contains })

    protected def maxFringe(newGraph:Graph, newHistory:List[Cluster]) =
      {
        // if all of its parents are either included in the history or optionals, it is on the fringe
        val parents = optional ::: newHistory
        newGraph.nodes.filter { node =>
          newGraph.predecessorsOf(node).forall(parents.contains)
        }
      }

    protected def computeExcluded(history: List[Cluster], melinks: List[MutualExcl]): List[Cluster] =
      {
        history.flatMap(s => melinks.filter(m => m.c1 == s).map(m => m.c2) :::
          melinks.filter(m => m.c2 == s).map(m => m.c1))
      }

    //    protected def nextFringe(step: Cluster, melinks: List[MutualExcl], optionals: List[Cluster]): Walk = {
    //      if (!fringe.contains(step)) throw new RuntimeException("Illegal Step: " + step)
    //
    //      val newHistory = step :: history
    //      var excluded = computeExcluded(List(step), melinks).filter(selfGraph.nodes contains)
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

    /**
     * if all direct predecessors of an event is in the event list, add that event to the event list
     * continue adding events until no such event exists
     */
    protected def findTransitiveClosure(graph: Graph, events: List[Cluster]): List[Cluster] =
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

    private var count = 0
    var debug = false

    private def nextId() = {
      count += 1
      count
    }

    def totalGenerated() = count

    def apply(graph: Graph) = {
      val me = graph.mutualExcls

      // starting point:
      var sources = graph.nodes.filterNot(n => graph.links.exists(l => l.target == n))
      //println(sources.map(_.name).mkString("sources 1: ", "\n", ""))
      val ends = graph.nodes.filterNot(n => graph.links.exists(l => l.source == n))
      //println(ends.map(_.name).mkString("ends : ", "\n", ""))
      //readLine()

      val graphIndex = new GraphIndex(graph)

      // remove from the graph nodes without predecessors that are not sources
      //graph = eGraph.removeIrregularSourceEnds()

      val optionals = findOptionals(graph)

      //println("optionals 1 : " + optionals.map(_.name).mkString("\n"))
      val finalGraph = graph.addSkipLinks(optionals)

      sources = finalGraph.nodes.filter(n => (!sources.contains(n)) &&
        finalGraph.links.filter(l => l.target == n).map(_.source).forall(optionals contains)) ::: sources

      //println(sources.map(_.name).mkString("sources 2 : ", "\n", ""))

      new Walk(finalGraph, sources, ends, me, optionals, sources)
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
  }
}