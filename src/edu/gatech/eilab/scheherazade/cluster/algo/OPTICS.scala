package edu.gatech.eilab.scheherazade {
  
  import io._
  import data._
  import scala.collection.mutable.PriorityQueue
  import scala.math.Ordered
  import cluster.gui.ReachPlot
  import java.io._
  package cluster.algo {

    object OPTICS {

      var loose = false
      final val MAX_DISTANCE = Double.PositiveInfinity
      final val UNDEFINED: Double = Double.PositiveInfinity
      var minClusterSize: Int = 1

      var plot: ReachPlot = null

      case class Point(val id: Int) extends Ordered[Point] {
        var visited: Boolean = false
        var core: Double = -2
        var reachability: Double = UNDEFINED
        var localMaximum = false
        var plotId = 0
        var previous: Point = null
        var next: Point = null

        override def compare(that: Point): Int = {
          if (this.reachability == UNDEFINED && that.reachability != UNDEFINED) -1 // UNDEFINED is greater than everything
          else if (this.reachability != UNDEFINED && that.reachability == UNDEFINED) 1
          else if (this.reachability < that.reachability) 1
          else if (this.reachability == that.reachability) 0
          else -1
        }
      }

      def main(args: Array[String]) {
        var matrix = HierarchicalClusterer.readCSV("movieOriginalSimilarity.csv")
        var sentences: List[Sentence] = SimpleParser.parseStories("./data/movie/movieSimpleStories.txt").flatMap(_.members).toList
        var max: Double = 0
        val distance = matrix.map { a =>
          a.map { value =>
            if (value != 0) {
              val v = 10 / value
              if (v > max) max = v
              v
            } else Double.PositiveInfinity
          }
        }

        val clusterList = cluster(distance, max, 4, sentences)
        //val text = clusterList.filter(_.members.length > 1).map(_.toHexSeparatedString()).mkString
        //println()

        //println(text)
      }

      def cluster2(similarity: Array[Array[Double]], epsilon: Double, minPts: Int, sentences: List[Sentence]): List[Cluster] = {
        val INFINITY = 10000
        val n = similarity.length
        var coreDist = Array.fill[Double](n)(0)
        var reachDist = Array.fill[Double](n)(INFINITY)

        // find core distances
        for (i <- 0 until n) {
          val sorted = similarity(i).sortWith(_ < _)
          coreDist(i) = sorted(minPts)
        }

        //for(i <- 0 until n)
        //	println((i + 1) + " " + coreDist(i))

        var order = List[Int]()
        var seeds = (0 until n).toList

        var index = 0
        while (seeds != Nil) {
          var ob = index
          seeds = seeds filter (_ != index)
          order = ob :: order

          val mm = seeds.map(similarity(ob)(_)).map(v => math.max(v, coreDist(ob)))

          //val ii = seeds.map(reachDist(_)).zip(mm).map { case (x, y) => x > y }

          //          for (i <- 0 until mm.length)
          //            println((i + 1) + " " + ii(i))
          //          System.exit(1)
          //println("mm dimension = " + mm.size + " ii dimension =" + ii.size)

          for (i <- 0 until seeds.length) {
            val s = seeds(i)
            if (reachDist(s) > mm(i)) reachDist(s) = mm(i)
          }

          if (seeds != Nil) {
            val min = seeds.map(i => reachDist(i)).min
            index = seeds.filter(s => reachDist(s) == min).head
            //println("next index = " + index)
          }
        }

        reachDist(0) = 1.02 * reachDist.tail.max
        order = order.reverse
        /****************** END OF OPTICS CODE **********************/
        val sorted = order.map(i => sentences(i))

        val points = for (i <- order) yield {
          val p = new Point(i)
          p.core = coreDist(i)
          p.reachability = reachDist(i)
          p
        }

        if (points.length > 1) {
          points(0).next = points(1)
          points.last.previous = points(points.length - 2)
        }

        for (i <- 1 until points.length - 1) {
          points(i).previous = points(i - 1)
          points(i).next = points(i + 1)
        }
        
        val larray = points.toArray
        plot = new ReachPlot(larray)
        plot.show
        val root = interpret(larray, minPts)

        //markLeaves(root, sentences)
        //println("required: (555, 613) = " + similarity(555)(613) + " (485, 613) = " + similarity(485)(613) )

        inferClusters(root, sentences)
        /*
        println(sorted.map(_.toShortString()).mkString("\n"))
        
        val no = sorted.map(_.id)

        val correct = scala.io.Source.fromFile("sentences.txt").getLines().map {
          line =>
            val cut = line.indexOf(" ")
            line.substring(0, cut).toInt
        }.toArray

        //val good = (0 until no.length).forall(i => no(i) == correct(i))
        //(0 until no.length).foreach(i => if (no(i) != correct(i)) println( no(i) + " <> " + correct(i)))
        print("GOOD = " + good)
        */
      }

      def cluster(similarity: Array[Array[Double]], epsilon: Double, minPts: Int, sentences: List[Sentence]): List[Cluster] = {

        val length = similarity.length
        val points = (0 until length).map(new Point(_)).toArray
        for (p <- points) p.core = computeCore(p, epsilon, minPts, similarity)
        /*for each point p of DB
       p.reachability-distance = UNDEFINED
    */
        var ordered = List[Point]()

        // for each unprocessed point p of DB
        for (i <- 0 until length) {
          if (!points(i).visited) {
            val p = points(i)

            // N = getNeighbors(p, eps)
            val neighbors = getNeighbors(p, epsilon, similarity, points)

            // mark p as processed
            p.visited = true

            // output p to the ordered list
            ordered = p :: ordered

            /*
         * Seeds = empty priority queue
	       if (core-distance(p, eps, Minpts) != UNDEFINED)
	          update(N, p, Seeds, eps, Minpts)
	          for each next q in Seeds
	             N' = getNeighbors(q, eps)
	             mark q as processed
	             output q to the ordered list
	             if (core-distance(q, eps, Minpts) != UNDEFINED)
	                update(N', q, Seeds, eps, Minpts)
          */

            // Seeds = empty priority queue
            var seeds = new PriorityQueue[Point]()
            // if (core-distance(p, eps, Minpts) != UNDEFINED)
            if (p.core != UNDEFINED) {
              seeds = update(neighbors, p, seeds, epsilon, minPts, similarity)

              while (!seeds.isEmpty) {
                // debug: print the seeds queue
                //            var text = seeds.map(x => x.reachability).min
                //            println("*************MIN*************" + text)
                //            println("*************head*************" + seeds.head.reachability)
                val q = seeds.dequeue()
                val qNeighbors = getNeighbors(q, epsilon, similarity, points)
                q.visited = true
                ordered = q :: ordered
                if (q.core != UNDEFINED)
                  seeds = update(qNeighbors, q, seeds, epsilon, minPts, similarity)
              }
            }
          }
        }

        var list = ordered.reverse // this is the reachability plot

        //println("list \n" + list.map(_.reachability).mkString("\n"))

        if (list.length > 1) {
          list(0).next = list(1)
          list.last.previous = list(list.length - 2)
        }

        for (i <- 1 until list.length - 1) {
          list(i).previous = list(i - 1)
          list(i).next = list(i + 1)
        }
        //println(list.size)
        //    val min = list.map(_.reachability).min
        //    val max = list.map(_.reachability).filterNot(_ == Double.PositiveInfinity).max
        //    val cutoff = (max - min) * 0.5 + min 
        //    list = list.head :: list.tail.filter(_.reachability < cutoff)
        //println(list.size)

        /*
        println("**************************")
        for (p <- list) {
          println(p.reachability)
        }
        println("**************************")
         */
        val larray = list.toArray
        plot = new ReachPlot(larray)
        plot.show
        val root = interpret(larray, minPts)

        //markLeaves(root, sentences)
        //println("required: (555, 613) = " + similarity(555)(613) + " (485, 613) = " + similarity(485)(613) )

        val pw1 = new PrintWriter(new FileOutputStream("simMatrix.txt"))
        val pw2 = new PrintWriter(new FileOutputStream("sentences.txt"))

        for (i <- 0 until similarity.length) {
          for (j <- 0 until similarity(0).length) {
            pw1.print(similarity(i)(j).toString.substring(0, 5) + ", ")
          }
          pw1.println()
        }

        for (i <- 0 until sentences.length) {
          val str = sentences(i).toShortString().replaceAll("\\)", " ").replaceAll("\\(S", "")
          pw2.println(str)
        }
        pw1.close()
        pw2.close()
        inferClusters(root, sentences)

        //Nil
        //    var start = 0
        //    var end = 0
        //    var clusterList = List[Cluster]()
        //    var curList: List[Sentence] = null
        //    // now we need to extract the clusters
        //    for (i <- 0 until list.length) {
        //      val p = list(i)
        //      //println(p.id + "/" + sentences(p.id).id + ": " + p.reachability)
        //      if (p.reachability == UNDEFINED) {
        //
        //        //println("starting a cluster with " + p.id + " " + sentences(p.id).id)
        //        if (curList != null) {
        //          end = i - 1
        //          regions = (start, end) :: regions
        //          clusterList = new Cluster("name", curList) :: clusterList
        //        }
        //        curList = List[Sentence](sentences(p.id))
        //        start = i
        //      } else {
        //        //println("adding to cluster " + p.id + " " + sentences(p.id).id)
        //        curList = sentences(p.id) :: curList
        //      }
        //    }

        //    println(regions)
        //    clusterList = new Cluster("name", curList) :: clusterList
        //    //    
        //    clusterList.filter(_.members.length > 1)

      }

      def inferClusters(root: Node, sentences: List[Sentence]): List[Cluster] = {
        var regions = List[List[Point]]()

        def findLeaves(r: Node) {
          if (r.children == Nil) {

            var pts = r.rArray

            val reach = pts.map(_.reachability)

            val valid =
              reach.sliding(minClusterSize + 1).exists(l => l.head * 0.98 > l.tail.min) && // head is greater than min movie = 0.98
                reach.sliding(minClusterSize).exists(l => l.max < l.min * 1.4) // a relative flat area bestRobbery = 1.05, best movie = 1.4

            if (valid) {
              val max = reach.max
              val min = reach.min
              //val portion = if (loose) 0.35 else 0.3
              var goodPortion = pts.filter { x => x.reachability < (min + (max - min) * 0.5) }.toList // 0.5(in acs paper)-0.6 for movie 0.4 for robbery
              // divide the portions into continuous parts
              var additional = List[Point]()
              var separation = List[(Int, Int)]()
              // these indices are w.r.t in this region
              var start = 0
              var end = start
              for (i <- 0 until goodPortion.length - 1) {
                if (goodPortion(i).next == goodPortion(i + 1))
                  end += 1
                else {
                  separation = (start, end) :: separation
                  start = i + 1
                  end = start
                }
              }
              separation = (start, end) :: separation

              // add the point before the continuous parts only if it is of similar height with the point immediately after the part
              for (s <- separation) {
                var endInd = s._2 + 1
                var endPt = goodPortion(s._2).next
                val startPt = goodPortion(s._1).previous
                // the is the heigh cutoff
                var height = goodPortion(s._1).reachability

                if (endPt != null && startPt != null && startPt.reachability > goodPortion(s._1).reachability &&
                  ((endPt.reachability * 1.4 > startPt.reachability &&
                    endPt.reachability * 0.8 < startPt.reachability) || startPt.reachability == UNDEFINED)) {
                  //println("end = " + endPt.reachability + ", start = " + startPt.reachability + " thre = " + endPt.reachability * 1.3)
                  additional = startPt :: additional
                  height = startPt.reachability
                }

                //now try to add some points after the end point
                var break = false;
                while (endInd < pts.length && !break) {
                  if (pts(endInd).reachability <= height && pts(endInd).reachability >= pts(endInd).previous.reachability) {
                    additional = pts(endInd) :: additional
                    endInd += 1;
                  } else break = true;
                }
              }
              // add the starting points of each segment

              //          for (pt <- goodPortion) {
              //            if (!goodPortion.contains(pt.previous))
              //              starts = pt.previous :: starts
              //          }
              goodPortion = additional ::: goodPortion
              regions = goodPortion.distinct :: regions
            }
          } else r.children.foreach(findLeaves(_))
        }
        //println(minClusterSize)
        findLeaves(root)
        //plot.unmarkAll()
        plot.markRegionalPoints(regions.map { _.map { _.plotId } })

        var clusterList = List[Cluster]()

        for (r <- regions) {
          var members = List[Sentence]()
          println("@ a")
          for (pt <- r) {
            val text = sentences(pt.id).toShortString().replaceAll("\\(S", "").replaceAll("\\)", "")
            println(text) // + " : " + sentences(pt.id).location)
            members = sentences(pt.id) :: members
          }
          println("###")
          val name = members.last.tokens.map(_.word).mkString(" ")
          val c = new Cluster(name, members)
          members.foreach(_.cluster = c)
          clusterList = c :: clusterList
        }

        clusterList
      }

      def markLeaves(root: Node, sentences: List[Sentence]) {
        var leaves = List[Node]()

        def findLeaves(r: Node) {
          if (r.children == Nil) {
            leaves = r :: leaves
          } else r.children.foreach(findLeaves(_))
        }

        findLeaves(root)
        var r = List[(Int, Int)]()
        leaves.foreach { leaf =>
          //      println(leaf.rArray.map(pt => sentences(pt.id).toString()).mkString("\n"))
          //      println("###")

          val start = leaf.rArray.head.plotId
          val end = leaf.rArray.last.plotId
          for (p <- leaf.rArray)
            println(sentences(p.id).toShortString())

          println("###")
          r = (start, end) :: r
        }
        //println("leaves: " + r)

        plot.markRegions(r)
      }

      protected def update(neighbors: List[Point], p: Point, seeds: PriorityQueue[Point], eps: Double, minPts: Int, similarity: Array[Array[Double]]): PriorityQueue[Point] = {
        /*coredist = core-distance(p, eps, MinPts)
    for each o in N
       if (o is not processed)
          new-reach-dist = max(coredist, dist(p,o))
          if (o.reachability-distance == UNDEFINED) // o is not in Seeds
              o.reachability-distance = new-reach-dist
              Seeds.insert(o, new-reach-dist)
          else               // o in Seeds, check for improvement
              if (new-reach-dist < o.reachability-distance)
                 o.reachability-distance = new-reach-dist
                 Seeds.move-up(o, new-reach-dist)
                 */

        var nQueue = seeds.clone
        val coreDist = p.core
        for (o <- neighbors)
          if (!o.visited) {
            //println("processing neighbor of " + p.id + " : " + o.id)
            val newReachDist = math.max(coreDist, similarity(p.id)(o.id))
            //val newReachDist = similarity(p.id)(o.id)
            if (o.reachability == UNDEFINED) {
              //println(o.id + " new reachabitlity = " + newReachDist)
              o.reachability = newReachDist
              nQueue += o
            } else if (newReachDist < o.reachability) {
              o.reachability = newReachDist
              //println(o.id + " new reachabitlity = " + newReachDist)
              nQueue = (nQueue filterNot { _.id == o.id })
              nQueue += o
            }
          }
        nQueue
      }

      //  protected def coreDistance(p: Point, eps: Double, minPts: Int, similarity: Array[Array[Double]]): Double =
      //    {
      //      if (p.core == -2) {
      //        val i = p.id
      //        val sorted = similarity(i).sortWith(_ < _)
      //        // check if there are minPts within epsilon
      //        if (sorted(minPts - 1) > eps)
      //          p.core = -1
      //        else
      //          p.core = sorted(minPts - 1)
      //      }
      //      p.core
      //    }

      protected def computeCore(p: Point, eps: Double, minPts: Int, similarity: Array[Array[Double]]): Double =
        {
          val i = p.id
          val sorted = similarity(i).sortWith(_ < _)
          // check if there are minPts within epsilon
          if (sorted(minPts - 1) > eps)
            UNDEFINED
          else
            sorted(minPts - 1)
        }

      protected def getNeighbors(p: Point, eps: Double, similarity: Array[Array[Double]], points: Array[Point]): List[Point] =
        {
          points filter { i: Point => similarity(p.id)(i.id) < eps } toList
        }

      class Node(var localMaxima: List[Point], val rArray: Array[Point], var Parent: Node = null) {
        var children: List[Node] = List[Node]()
        var splitPt: Point = null
      }

      def interpret(rArray: Array[Point], minPts: Int): Node =
        {
          import scala.collection.mutable.ListBuffer

          for (i <- 0 until rArray.length)
            rArray(i).plotId = i

          var localMaxima = new ListBuffer[(Int, Point)]()

          minClusterSize = minPts + 1

          // first, find all local maximum
          for (idx <- 0 until rArray.length) {
            val point = rArray(idx)
            val left = math.max(0, idx - minClusterSize).toInt
            val right = math.min(rArray.length - 1, idx + minClusterSize).toInt
            var (nbLeft, nbRight) = rArray.drop(left).dropRight(rArray.length - 1 - right).splitAt(idx - left)
            nbRight = nbRight.drop(1)

            val leftDefined = {
              if (!nbLeft.isEmpty) point.reachability != nbLeft.last.reachability
              else true
            }
            val rightDefined = {
              if (!nbRight.isEmpty) point.reachability != nbRight.head.reachability
              else true
            }

            val maxima = (rightDefined || leftDefined) &&
              (nbLeft ++ nbRight).forall { p =>
                if (rArray(idx).reachability == UNDEFINED) true // undefined points are always local maxima
                else p.reachability != UNDEFINED && p.reachability <= rArray(idx).reachability
              }

            if (maxima) {
              localMaxima += ((idx, point))
              point.localMaximum = true
              //println("****\n" + (nbLeft ++ nbRight).map(_.reachability).mkString("\n"))
              //println("lm: " + array(i).reachability + "****\n")
            }
          }

          val lm = localMaxima.sortBy(_._2).toList

          //println(lm.map(p => p._2.plotId + " " + p._2.reachability).mkString("\n"))
          val sortedMaxima = lm.map(_._2)
          val root = new Node(sortedMaxima, rArray, null)
          clusterTree(root, null)
          //System.exit(-1)

          //      val buffer = ListBuffer[Node]()
          //      var start = 0
          //      for(x <- localMaxima.tail)
          //      {
          //        val end = x._1
          //        val interval = (start until end).map(rArray(_)).toArray
          //        val n = new Node(Nil, interval, root)
          //        root.children = n :: root.children 
          //        start = end
          //      }

          root
        }

      def clusterTree(n: Node, parent: Node) {
        import scala.collection.mutable.ListBuffer

        //println("starting collection")

        val id1 = n.rArray.head.plotId
        val id2 = n.rArray.last.plotId
        plot.markRegions(List((id1, id2)))
        plot.sendMessage("processing a node: " + id1 + " - " + id2)
        plot.unmarkRegions(List((id1, id2)))

        if (n.localMaxima.isEmpty) {
          //println("no local maxima in this region")
          plot.disableRegions(List((id1, id2)))
          parent.children = n :: parent.children
          return
        }

        //println("from " + n.rArray.head.id + " to " + n.rArray.last.id)

        n.splitPt = n.localMaxima.head

        val rArray = n.rArray // reachability array
        var maxima = n.localMaxima.tail
        val split = rArray.indexWhere(_ == n.splitPt)
        val splitPt = n.splitPt
        n.localMaxima = n.localMaxima.tail

        //println("split pt = " + splitPt + " " + splitPt.plotId + " split = " + split)
        // cannot use the names N1, N2. Bug?
        var (n1, n2) = n.rArray.splitAt(split)
        //n2 = n2.drop(1)

        var L1 = new ListBuffer[Point]()
        var L2 = new ListBuffer[Point]()
        for (pt <- maxima) {
          if (pt.plotId < splitPt.plotId) L1 += pt
          else if (pt.plotId > splitPt.plotId) L2 += pt
        }

        //println("L1 " + L1.map(_.plotId).mkString(", "))
        //println("L2 " + L2.map(_.plotId).mkString(", "))

        //println("left size = " + n1.size)
        //println("right size = " + n2.size)
        //println("right = " + { if (n2.isEmpty) "()" else n2.head.id + " to " + n2.last.id })
        val kid1 = new Node(L1.toList, n1)
        val kid2 = new Node(L2.toList, n2)
        val kids = ListBuffer(kid1, kid2)

        var l = List[(Int, Int)]()
        if (!n1.isEmpty) l = (n1.head.plotId, n1.last.plotId) :: l
        if (!n2.isEmpty) l = (n2.head.plotId, n2.last.plotId) :: l
        plot.markRegions(l)
        plot.sendMessage("split")
        plot.unmarkRegions(List((id1, id2)))

        // average reachability of the left region
        val (leftAvg, leftDev: Double) = {
          val region = (0 until split).map(rArray(_).reachability).filterNot(_ == UNDEFINED)
          if (region.length > 0) {
            val min = region.min
            val v = region.map(_ - min)
            (region.sum / region.length, v.sum / v.length)
          } else (UNDEFINED, 0.0)
        }

        // average reachability of the right region
        val (rightAvg, rightDev: Double) = {
          val region = (split + 1 until rArray.length).map(rArray(_).reachability).filterNot(_ == UNDEFINED)
          if (region.length > 0) {
            val min = region.min
            val v = region.map(_ - min)
            (region.sum / region.length, v.sum / v.length)
          } else (UNDEFINED, 0.0)
        }
        //println(" regions left avg = " + leftAvg + ", right avg = " + rightAvg + ", threshold = " + splitPt.reachability * 0.85)
        // signficance == leftAvg and rightAvg < 75% of split point
        if (leftAvg > splitPt.reachability * 0.85 && rightAvg > splitPt.reachability * 0.85) {
          //println()
          //val portion = 0.75
          //if ((leftAvg + portion * leftDev) > splitPt.reachability && (rightAvg + portion * rightDev) > splitPt.reachability) {
          // this is not significant. Continue to the next
          //println("not significant: " + leftAvg + ", " + rightAvg + " > " + splitPt.reachability * 0.85)
          if (!n.localMaxima.isEmpty) {
            n.splitPt = n.localMaxima.head
            n.localMaxima = n.localMaxima.tail
            clusterTree(n, parent)
          } else {
            /* There is no more maxima 
         * stop splitting and add this node to its parent
         */
            parent.children = n :: parent.children
          }
        } else {
          // remove clusters that are too small
          if (n1.length < minClusterSize) {
            //println("kid 1 too small")
            kids -= kid1
          }
          if (n2.length < minClusterSize) {
            //println("kid 2 too small")
            kids -= kid2
          }
          if (kids.isEmpty) {
            //println("both kids too small")
            parent.children = n :: parent.children
            return
          }

          val nextParent = {
            if (parent != null && approximateHeight(n, parent)) {
              //println("using parent as parent")
              parent
            } else {
              if (parent != null)
                parent.children = n :: parent.children
              //println("using current node as parent")
              n
            }
          }

          for (kid <- kids) {
            //println("going to kid " + kid.rArray.head.id + " to " + kid.rArray.last.id)
            clusterTree(kid, nextParent)
          }
        }
      }

      // TODO
      def approximateHeight(n: Node, parent: Node): Boolean = {
        import utils.Calculator
        val c = new Calculator()
        val v = parent.rArray.map(_.reachability)
        c.add(v)
        val stdDev = c.stdDev()

        math.abs(n.splitPt.reachability - parent.splitPt.reachability) < stdDev
      }

    }
  }
}


