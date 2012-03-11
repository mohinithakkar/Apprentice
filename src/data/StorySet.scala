package data

import scala.annotation.target.field
import scala.collection.JavaConverters._

import java.util.{ List => JList }
import javax.xml.bind.annotation._
import javax.xml.bind.annotation.adapters._
import javax.xml.bind._

import javax.xml.transform.stream.StreamResult
import java.io._

@XmlRootElement()
@XmlAccessorType(XmlAccessType.FIELD)
class StorySet(
  @XmlElement(required = true) val name: String,
  @XmlElement(name = "story-list", required = true) var storyList: List[Story]) {

  //type xmlElement = XmlElement @field
  //type xmlTypeAdapter = XmlJavaTypeAdapter @field
  
  def getStoryList(): JList[Story] = new java.util.ArrayList(storyList.asJava)
  def setStoryList(jlist: JList[Story]) = storyList = jlist.asScala.toList

  private def this() = this("", List())
}

class TokenListAdapter extends ScalaListAdapter[Token]
class SentenceListAdapter extends ScalaListAdapter[Sentence]
//class StoryListAdapter extends ScalaListAdapter[Story]

class StoryListAdapter extends XmlAdapter[java.lang.Iterable[Story], List[Story]] {
  def marshal(list: List[Story]): java.lang.Iterable[Story] = list.asJava
  def unmarshal(jlist: java.lang.Iterable[Story]): List[Story] = jlist.asScala.toList
}

class ScalaListAdapter[T](implicit m: Manifest[T]) extends XmlAdapter[JList[T], List[T]] {
  def marshal(list: List[T]): JList[T] = new java.util.ArrayList(list.asJava)
  def unmarshal(jlist: JList[T]): List[T] = jlist.asScala.toList
}

object PersistenceTest extends App {

  val sent1 = new Sentence(1, List(Token("John", "NN")))
  val sent2 = new Sentence(1, List(Token("Mary", "NN")))

  val s1 = new StorySet("movie", List(new Story(Array(sent1)), new Story(Array(sent2))))
  val context = JAXBContext.newInstance(classOf[StorySet])

  val marshaller = context.createMarshaller();

  marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

  marshaller.marshal(s1, System.out);

  val context2 = JAXBContext.newInstance(classOf[Sentence])
  val marshaller2 = context2.createMarshaller();
  marshaller2.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
  marshaller2.marshal(sent1, System.out);

  //jaxbMarshaller.marshal(customer, System.out);

  context.generateSchema(new SchemaOutputResolver {
    def createOutput(namespaceUri: String, suggestedFileName: String) = {
      val res = new StreamResult(System.out)
      res.setSystemId(suggestedFileName)
      res
    }
  })
}