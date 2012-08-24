package edu.gatech.eilab.scheherazade.data

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

case class Dependency(val gov: Token, val dep: Token, val relation: String, val specific: String, val depth: Int) {

  override def toString() = relation + (if (specific == "") "" else "_" + specific) + "(" + gov.word + ", " + dep.word + ", " + depth + ")"
  
  lazy val longName = if (specific != "" && specific != null) relation + "_" + specific else relation
  
  def toXML() = <Dependency>
  		<governorId>{gov.id}</governorId>
  		<dependentId>{dep.id}</dependentId>
		<relation>{relation}</relation>
		<specific>{specific}</specific>
		<depth>{depth}</depth>
  	</Dependency>
}


object Dependency {

  def addDepths(deps: List[Dependency]): List[Dependency] =
    {
      var allDeps = deps
      val listbuffer = new ListBuffer[Dependency]

      val rootNode = deps.find(x => !deps.exists(d => x.gov == d.dep)).get.gov

      val queue = new Queue[(Dependency, Int)]()
      val firstLayer = deps.filter(_.gov == rootNode)
      allDeps --= firstLayer
      queue ++= (firstLayer.map{(_, 1)})

      while (!queue.isEmpty) {
        val (typedDep, depth) = queue.dequeue()
        listbuffer += new Dependency(typedDep.gov, typedDep.dep, typedDep.relation, typedDep.specific, depth)
        val nextLayer = allDeps.filter(_.gov == typedDep.dep)
        allDeps = allDeps -- nextLayer
        queue ++= nextLayer.map((_, depth + 1))
      }

      listbuffer.toList

    }
}