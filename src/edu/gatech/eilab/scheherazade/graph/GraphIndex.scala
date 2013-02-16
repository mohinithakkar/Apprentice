package edu.gatech.eilab.scheherazade.graph

class GraphIndex(graph: Graph) {

  val node2Num = {
    val sorted = graph.nodes.sortWith((x, y) => x.name > y.name) // introduce an arbitrary ordering between the clusters
    val num = 0 until graph.nodes.length
    (sorted zip num).toMap
  }

  val num2Node = {
    node2Num.map { case (x, y) => (y, x) }
  }
}