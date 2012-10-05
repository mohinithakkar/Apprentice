package edu.gatech.eilab.scheherazade.data
import java.util.Properties

class Relation(val source: Cluster, val target: Cluster, val trueInstance: Int, val totalObservations: Int) //extends Ordered[Relation] 
{

  override def equals(o: Any) = o match {
    case that: Relation => this.source == that.source && this.target == that.target
    case _ => false
  }

  override def hashCode(): Int = (source.hashCode * 17 + target.hashCode * 23) / 41

  val prob: Double = {
    if (totalObservations == 0)
      0
    else trueInstance.toDouble / totalObservations
  }

  override def toString() = source.name + " -> " + target.name + " (" + trueInstance + " / " + totalObservations + ")"

  def addEvidence(positive: Int, negative: Int) = Relation(source, target, trueInstance + positive, totalObservations + positive + negative)

  def confidence = 1 - oneTail(trueInstance, totalObservations, 0.5)

  private def oneTail(k: Int, n: Int, p: Double): Double =
    (k to n) map (binomial(_, n, p)) sum

  private def binomial(k: Int, n: Int, p: Double): Double =
    choose(n, k).toDouble * math.pow(p, k) * math.pow(p, n - k)

  private def choose(n: Int, k: Int): BigInt = (k + 1 to n).foldLeft(BigInt(1))(_ * BigInt(_)) / (1 to n - k).foldLeft(BigInt(1))(_ * BigInt(_))
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

/**
 * the Link class includes both temporal and causal links
 * the default is temporal if the kind field is not specified
 */
class Link(val source: Cluster, val target: Cluster, val kind: String = "T") {
  require(kind == "T" || kind == "C")

  override def toString = source.name.replace(" ", "_") + " -> " + target.name.replace(" ", "_")
  override def equals(o: Any) = o match {
    case other: Link => this.source == other.source && this.target == other.target && this.kind == other.kind
    case _ => false
  }
  override def hashCode(): Int = (source.hashCode() + target.hashCode() + kind.hashCode()) * 19 / 97
  def isTemporal = (kind == "T")
  def isCausal = (kind == "C")
}

class MutualExcl(val c1: Cluster, val c2: Cluster) {
  override def toString() = "ME: " + c1.name + " -/- " + c2.name
}