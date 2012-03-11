package data
import scala.annotation.target.field
import javax.xml.bind.annotation._
import javax.xml.bind.annotation.adapters._
import javax.xml.bind._

@XmlRootElement(name="token")
@XmlAccessorType(XmlAccessType.FIELD)
case class Token(
    @XmlElement @field val word: String, 
    @XmlElement @field val pos: String) {
  
  private def this() = this("", "")
}

@XmlRootElement(name="sentence")
@XmlAccessorType(XmlAccessType.FIELD)
case class Sentence(
    @XmlElement @field val id: Int, 
    @XmlJavaTypeAdapter(classOf[TokenListAdapter]) @field val content: List[Token], 
    @XmlElement @field var next: Sentence = null, 
    @XmlElement @field var cluster: Cluster = null) {
  
  private def this() = this(0, Nil)
  
  override def toString(): String =
    {
      "(S" + id + ") " + content.map { t => t.word + "\\" + t.pos }.mkString(" ")
    }

  def toShortString(): String =
    {
      "(S" + id + ") " + content.map { _.word }.mkString(" ")
    }
}

@XmlRootElement(name="cluster")
@XmlAccessorType(XmlAccessType.FIELD)
class Cluster(
    @XmlElement val name: String, 
    val members: List[Sentence]) {
  
  private def this() = this("", Nil)
  
  override def toString(): String =
    {
      "Cluster \"" + name + "\": [" + members.map(_.id).mkString(",") + "]"
    }
}

@XmlRootElement(name="story")
@XmlAccessorType(XmlAccessType.FIELD)
class Story(
    @XmlElement val members: Array[Sentence]) {
  override def toString(): String =
    {
      "Story (" + members(0).id + ", " + members.last.id + ")"
    }

  private def this() = this(Array())
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