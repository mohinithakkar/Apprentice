package edu.gatech.eilab.scheherazade.data

import edu.stanford.nlp.trees._

case class Token(
  val id: Int, val word: String, val pos: String, val lemma: String, val ner: String) extends XStreamable

object Token {
  def apply(word: String, pos: String): Token = new Token(0, word, pos, "", "")
}

case class Sentence(
  val id: Int,
  val tokens: Array[Token],
  var parse: Tree,
  var deps: List[Dependency],
  var location: Double,
  var next: Sentence = null,
  var cluster: Cluster = null) extends XStreamable {

  //var location:Double = 0

  override def toString(): String =
    "(S" + id + ") " + tokens.map { t => t.word + "/" + t.pos }.mkString(" ")

  @deprecated("use toSimpleString instead", "since 1 Jan 2013")
  def toShortString(): String =
    {
      "(S" + id + ") " + tokens.map { _.word }.mkString(" ")
    }
    
  def toSimpleString() = 
    id + " " + tokens.map{ _.word }.mkString(" ")
  

  // these two override methods are only temporary. Should delete after Mar 15
  override def equals(o: Any) = o match {
    case s: Sentence => this.id == s.id
    case _ => false
  }

  override def hashCode() = id.hashCode()
}

object Sentence {
  def apply(id: Int, tokens: Array[Token], loc: Double) = new Sentence(id, tokens, null, null, loc)
  def apply(id: Int, tokens: Array[Token]) = new Sentence(id, tokens, null, null, 0)
}

class Cluster(
  val name: String,
  val members: List[Sentence]) extends XStreamable {

  def size(): Int = members.size

  def coherence(matrix: Array[Array[Double]]): Double = {
    var sum = 0.0
    val combinations = members.combinations(2).toArray
    for (comb <- combinations) {
      val sim = matrix(comb(0).id)(comb(1).id)
      sum += sim
    }
    sum / combinations.size
  }
  override def toString(): String =
    {
      "Cluster \"" + name + "\": [" + members.map(_.id).mkString(",") + "]"
    }

  override def equals(o: Any) = o match {
    case that: Cluster => this.name == that.name
    case _ => false
  }

  override def hashCode(): Int = ("Cluster" + name).hashCode

  def toHexSeparatedString() =
    {
      var text =
        members map { s =>
          val str = s.toString
          s.id + " " + str.substring(str.indexOf(")") + 2)
        } mkString ("\n")

      text += "\n###\n"
      text
    }
}

class Story(
  val members: Array[Sentence]) extends XStreamable {
  override def toString(): String =
    {
      if (members.isEmpty)
        "Story ()"
      else
        "Story (" + members.head.id + ", " + members.last.id + ")"
    }

  override def equals(o: Any): Boolean = o match {
    case that: Story => this.members == that.members
    case _ => false
  }

  override def hashCode() = ("Story".hashCode * 29 + members.hashCode * 53) / 107

  def addStoryLocation() {
    for (i <- 0 until members.length) {
      val s = members(i)
      val location = i.toDouble / (members.length - 1)
      s.location = location
    }
  }

  def size(): Int = members.length
}

//class ClusterLink(val source: Cluster, val target: Cluster, var count: Int = 0) extends Ordered[ClusterLink] with XStreamable {
//
//  override def equals(that: Any): Boolean =
//    that match {
//      case link: ClusterLink => this.source == link.source && this.target == link.target
//      case _ => false
//    }
//
//  def increment() {
//    count += 1
//  }
//
//  override def toString() = source.name + ", " + target.name + ", " + count
//
//  override def compare(that: ClusterLink): Int =
//    {
//      if (this.count < that.count) 1
//      else if (this.count == that.count) 0
//      else -1
//    }
//}