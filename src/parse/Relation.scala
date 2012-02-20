package parse
import data._

class Relation(val source: Cluster, val target: Cluster, val trueInstance: Int, val totalObservations: Int) extends Ordered[Relation] {

  val upper: Double = (trueInstance + Relation.UNOBSERVED).toDouble / (totalObservations + Relation.UNOBSERVED)
  val lower: Double = trueInstance.toDouble / (totalObservations + Relation.UNOBSERVED)
  val confidence: Double = 1 - (upper - lower)
  val prob: Double =
    if (totalObservations == 0)
      0
    else trueInstance.toDouble / totalObservations // INCORRECT: (upper + lower) / 2

  override def toString() = source.name + " -> " + target.name + " (" + trueInstance + ", " + totalObservations + ") (" + "%1.3f".format(prob) + "," + "%1.3f".format(confidence) + ")"

  override def compare(that: Relation): Int =
    {
      if (this.confidence < that.confidence) 1
      else if (this.confidence > that.confidence) -1
      else if (this.prob > that.prob) -1
      else if (this.prob == that.prob) 0
      else 1
    }

  def addEvidence(positive: Int, negative: Int) = Relation(source, target, trueInstance + positive, totalObservations + positive + negative)
}

object Relation {

  val UNOBSERVED: Int = 4 // PARAMETER S: number of hidden observations

  def apply(source: Cluster, target: Cluster, trueInstance: Int, totalObservations: Int) = new Relation(source, target,
    trueInstance, totalObservations)
}

class Link(val source: Cluster, val target: Cluster) {
  override def toString = source.name.replace(" ", "_") + " -> " + target.name.replace(" ", "_")
  override def equals(o:Any) = o match
  {
    case other:Link => this.source == other.source && this.target == other.target
    case _ => false
  }
}