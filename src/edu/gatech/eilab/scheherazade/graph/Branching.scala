package edu.gatech.eilab.scheherazade
import collection.mutable.ListBuffer
import data._
import collection.mutable.HashMap
import main._
import java.io._
package graph {
  object Branching {

    def discover(graph: Graph) {

      val effiGraph = graph.makeEfficient()
      var allCuts = ListBuffer[List[Cluster]]()
      var queue = collection.mutable.Queue[EfficientGraph]()

      queue += effiGraph
      while (!queue.isEmpty) {

        var currentGraph = queue.dequeue()
        //currentGraph.draw("currentGraph")
        var verCuts = currentGraph.minVertexCut()
        //println("vertex cuts: ")

        //printCuts(verCuts)
        // each of these cuts must have sufficient fan-in and fan-out
        verCuts = filterFanInFanOut(verCuts, currentGraph)
        val length = verCuts.length
        // this is supposed to sort the cuts in the order of the graph
        verCuts = verCuts.sortWith { (x, y) => x.exists { x1 => y.exists { y1 => currentGraph.numPaths(x1, y1) > 0 } } }

        //allCuts ++= verCuts
        val mergedWith = collection.mutable.HashMap[List[Cluster], List[Cluster]]()

        println("vertex cuts after considering fan-in fan-out: ")
        printCuts(verCuts)
        // now filter out the cuts that have only one path in between

        for (i <- 0 until length; j <- i + 1 until length) {
          val early = verCuts(i)
          val late = verCuts(j)
          var max = {
            for (u <- early; v <- late) yield currentGraph.numPaths(u, v)
          }.max

          // we do not need to consider the reverse direction because verCuts has been sorted.
          //      if (max == 0) max = {
          //      for (u <- late; v <- early) yield currentGraph.numPaths(u, v) 
          //      }.max 

          if (max == 1) {
            // one of the two must be taken out of the set of key cuts
            val earlyCnt = early.map(_.size).sum
            val lateCnt = late.map(_.size).sum
            if (earlyCnt > lateCnt) {
              // late merged with early. i is the index of early. We add late to the entry i of the hash table  
              //allCuts -= late 
              println("****************************")
              printCuts(List(late))
              println(" merged with ")
              printCuts(List(early))
              println("****************************")
              if (mergedWith.contains(early)) {
                mergedWith(early) = late ::: mergedWith(early)
              } else
                mergedWith(early) = late
            } else {
              // early merged with late
              //allCuts -= early
              println("****************************")
              printCuts(List(early))
              println(" merged with ")
              printCuts(List(late))
              println("****************************")

              if (mergedWith.contains(late)) {
                mergedWith(late) = early ::: mergedWith(late)
              } else {
                mergedWith(late) = early
              }
            }
          }

          if (max == 0) throw new GraphException("No routes between two vertex cuts. This is not supposed to happen. \n" +
            "The cuts are: " + early.map(_.name).mkString("(", ",", ")") + " and " + late.map(_.name).mkString("(", ",", ")"))

          //println(early.map(_.name).mkString("(", ",", ")") + " => " + late.map(_.name).mkString("(", ",", ")") + ": " + max)
        }

        val realCuts = verCuts -- mergedWith.values.toList
        if (!realCuts.isEmpty) {
          allCuts ++= realCuts
          // remove these key cuts from the graph.
          // the first graph is the graph from top to the first real Cut
          queue.enqueue(currentGraph.topCut(realCuts.head, mergedWith))
          // subsequent cuts are between the real cuts
          queue ++= currentGraph.middleCuts(realCuts, mergedWith)
          // the final cut
          queue.enqueue(currentGraph.lastCut(realCuts.last, mergedWith))
        }
        //    for(i <- 0 until queue.length)
        //    {
        //      println("interGraph ---- " + i)
        //      println(queue(i).nodes.map(_.name).mkString("(", ", " , ")"))
        //      queue(i).draw("interGraph"+i)
        //    }
      }
      println("*** final *** cuts ***")
      printCuts(allCuts)
      Thread.sleep(3000)
    }

    private def printCuts(cuts: Seq[List[Cluster]]) {
      println(cuts.map(_.map(x => x.name + ": " + x.size).mkString("(", ", ", ")")).mkString("\n"))
    }

    /**
     * A valid vertex cut must have sufficient fan in and fan out.
     * Specifically, we enforce max(fan-in, fan-out) > cut.size * 2
     *
     */
    private def filterFanInFanOut(cuts: List[List[Cluster]], graph: EfficientGraph): List[List[Cluster]] = {
      cuts.filter { one =>
        val nums = one.map { graph.node2Num(_) }
        val n = graph.nodes.length
        // must exclude links within the vertex set, i.e. within nums
        val totalFanIn = { for (i <- 0 until n; j <- nums if !nums.contains(i)) yield graph.adjacentMtx(i)(j) } sum

        val totalFanOut = { for (i <- nums; j <- 0 until n if !nums.contains(j)) yield graph.adjacentMtx(i)(j) } sum

        math.max(totalFanIn, totalFanOut) >= one.length * 2
      }
    }

    def main(args: Array[String]) {

      val storage = new File("graph.xml")
      var graph = if (storage.exists()) {
        println("reading from file " + storage.getAbsolutePath() + "...")
        val str = scala.io.Source.fromFile(storage).mkString
        XStream.fromXML(str).asInstanceOf[Graph]
      } else {
        val reader = new ConfigReader("configRobBest.txt")
        //var (stories, clusters) = reader.initOldDataFiltered()
        var (stories, clusters) = reader.initDataFiltered()
        val para = reader.properties.allParameters()(0)
        val gen = new GraphGenerator(stories, clusters)
        val g = gen.generate(para)("improved")._1
        val str = XStream.toXML(g)
        val pl = new PrintWriter(new BufferedOutputStream(new FileOutputStream(storage)))
        pl.println(str)
        pl.close()
        g
      }

      graph = graph.makeEfficient
      //graph.draw("robBest")
      discover(graph)
      //Thread.sleep(2000)
    }
  }
}