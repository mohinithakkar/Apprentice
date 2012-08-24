package edu.gatech.eilab.scheherazade.graph
import scala.collection.mutable.Queue$

/**
 * implements the Edmonds-Karp algorithm for finding maximum flow
 * in a direct s-t graph
 */

object EdmondsKarp {

  def compute(capacity: Array[Array[Int]], neighbors: Array[List[Int]], s: Int, t: Int): (Int, Array[Array[Int]]) =
    {
      var f = 0
      val n = capacity.length
      var flow = Array.ofDim[Int](n, n)

      var m = 0
      var P = Array[Int]()
      do {
        val temp = breathFirst(capacity, neighbors, s, t, flow)
        m = temp._1
        P = temp._2
        if (m > 0) {
          // we have found a non-zero augmenting path
          f += m
          // compute the path by tracking the parents backwards from the sink, t
          var v = t
          while(v != s)
          {
            var u = P(v)
            flow(u)(v) += m
            flow(v)(u) -= m
            v = u
          }
        }
      } while (m > 0)
        
        
        
      return (f, flow)
    }

  private def breathFirst(capacity: Array[Array[Int]], neighbors: Array[List[Int]], s: Int, t: Int, flow: Array[Array[Int]]): (Int, Array[Int]) =
    {
      val n = capacity.length
      var parent = Array.fill[Int](n)(-1)
      parent(s) = -2
      
      // the capacity of the route to a node, which is the index of this array
      var M = Array.fill[Int](n)(0)
      M(s) = Int.MaxValue

      var q = scala.collection.mutable.Queue[Int]()
      q.enqueue(s)
      
      while (!q.isEmpty) {
        val u = q.dequeue()
        for (v <- neighbors(u)) {
          //(If there is available capacity, and v is not seen before in search)
          if (capacity(u)(v) - flow(u)(v) > 0 && parent(v) == -1) {
            parent(v) = u
            // update the route capacity
            M(v) = math.min(M(u), capacity(u)(v) - flow(u)(v))
            if (v != t)
              q.enqueue(v)
            else
              return (M(t), parent)
          }
        }
      }
      
      // we have not found anything
      return (0, parent)
    }
  
  def main(args:Array[String])
  {
    testCase1()
  }
  
  private def testCase1()
  {
    // correct answer = 10
    val capacity = Array.ofDim[Int](5, 5)
    capacity(0)(1) = 5
    capacity(0)(2) = 7
    capacity(1)(2) = 3
    capacity(1)(3) = 7
    capacity(3)(2) = 5
    capacity(3)(4) = 4
    capacity(2)(4) = 6
    
    val neighbors = Array(List(1, 2), List(0, 2, 3), List(0, 1, 3, 4), List(1, 2, 4), List(2, 4))
    val s = 0
    val t = 4
    val x = EdmondsKarp.compute(capacity, neighbors, s, t)
    println(x._1)
    val str = x._2.map(_.mkString(",")).mkString("\n")
    println(str)
    
    if (x._1 == 10) println("Test case 1 passed!")
  }
}