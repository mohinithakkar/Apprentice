package edu.gatech.eilab.scheherazade.data
import java.util.Properties


class Relation(val source: Cluster, val target: Cluster, val trueInstance: Int, val totalObservations: Int) //extends Ordered[Relation] 
{

  //val upper: Double = (trueInstance + Relation.UNOBSERVED).toDouble / (totalObservations + Relation.UNOBSERVED)
  //val lower: Double = trueInstance.toDouble / (totalObservations + Relation.UNOBSERVED)
  //val confidence: Double = 1 - (upper - lower)
  val prob: Double =
    if (totalObservations == 0)
      0
    else trueInstance.toDouble / totalObservations

  //override def toString() = source.name + " -> " + target.name + " (" + trueInstance + ", " + totalObservations + ") (" + "%1.3f".format(prob) + "," + "%1.3f".format(confidence) + ")"
override def toString() = source.name + " -> " + target.name + " (" + trueInstance + " / " + totalObservations + ")"
//  override def compare(that: Relation): Int =
//    {
//      if (this.confidence < that.confidence) 1
//      else if (this.confidence > that.confidence) -1
//      else if (this.prob > that.prob) -1
//      else if (this.prob == that.prob) 0
//      else 1
//    }

//    override def compare(that: Relation): Int =
//    {
//      if (this.confidence < that.confidence) 1
//      else if (this.confidence > that.confidence) -1
//      else if (this.prob > that.prob) -1
//      else if (this.prob == that.prob) 0
//      else 1
//    }
  
  def addEvidence(positive: Int, negative: Int) = Relation(source, target, trueInstance + positive, totalObservations + positive + negative)
}

object Relation {

  //var UNOBSERVED: Int = 4 // PARAMETER S: number of hidden observations

//  def init(prop:java.util.Properties)
//  {
//    val str = prop.getProperty("s") 
//    if (str != null)
//    {
//      UNOBSERVED = str.trim.toInt
//      println("using the parameter: unobserved instances = " + UNOBSERVED)
//    }      
//  }
  
  def apply(source: Cluster, target: Cluster, trueInstance: Int, totalObservations: Int) = new Relation(source, target,
    trueInstance, totalObservations)
}

/** the Link class includes both temporal and causal links
 * the default is temporal if the kind field is not specified
 */
class Link(val source: Cluster, val target: Cluster, val kind:String = "T") {
  require(kind == "T" || kind == "C")
  
  override def toString = source.name.replace(" ", "_") + " -> " + target.name.replace(" ", "_")
  override def equals(o:Any) = o match
  {
    case other:Link => this.source == other.source && this.target == other.target && this.kind == other.kind
    case _ => false
  }
  override def hashCode():Int = (source.hashCode() + target.hashCode() + kind.hashCode()) * 19 / 97
  def isTemporal = (kind == "T")
  def isCausal = (kind == "C")
}