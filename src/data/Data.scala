package data

import edu.stanford.nlp.trees.Tree

case class Token(val id:Int, val word: String, val pos: String, val lemma:String, val ner:String) 
{
  def this(word: String, pos: String) = this(0, word, pos, "", "")
  def toXML() = <Token>
  	<id>{id}</id>
  	<word>{word}</word>
  	<pos>{pos}</pos>
  	<lemma>{lemma}</lemma>
  	<ner>{ner}</ner>
  </Token> 
}

object Token {
  def apply(word: String, pos: String):Token = new Token(word: String, pos: String)
}

case class Sentence(val id: Int, var tokens: Array[Token], var parse: Tree, var deps: List[Dependency], var next: Sentence = null, var cluster: Cluster = null) {

  override def toString(): String =
    {
      "(S" + id + ") " + tokens.map { t => t.word + "\\" + t.pos }.mkString(" ")
    }

  def toShortString(): String =
    {
      "(S" + id + ") " + tokens.map { _.word }.mkString(" ")
    }
  
  def toXML() = <Sentence>
		  <id>{id}</id>
		  <Tokens>{tokens.map{_.toXML}}</Tokens>
  			<Dependecies>{deps.map{_.toXML}}</Dependecies>
		  </Sentence>
}

object Sentence {
  def apply(id: Int, tokens: Array[Token]) = new Sentence(id, tokens, null, null)
}

class Cluster(val name: String, val members: List[Sentence]) {
  override def toString(): String =
    {
      "Cluster \"" + name + "\": [" + members.map(_.id).mkString(",") + "]"
    }
}

class Story(val members: Array[Sentence]) {
  override def toString(): String =
    {
      "Story (" + members(0).id + ", " + members.last.id + ")"
    }

}
class ClusterLink(val source: Cluster, val target: Cluster, var count: Int = 0) extends Ordered[ClusterLink] {
  
  override def equals(that: Any): Boolean =
    that match {
      case link: ClusterLink => this.source == link.source && this.target == link.target
      case _ => false
    }
  
  def increment()
  {
    count += 1
  }
  
  override def toString() = source.name + ", " + target.name + ", " + count

  override def compare(that:ClusterLink):Int =
  {
    if (this.count < that.count) 1
    else if (this.count == that.count) 0
    else -1
  }
}