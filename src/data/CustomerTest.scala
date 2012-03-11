package data

import scala.annotation.target.field
import scala.collection.JavaConverters._

import java.util.{ List => JList }
import javax.xml.bind.annotation._
import javax.xml.bind.annotation.adapters._
import javax.xml.bind._

import javax.xml.transform.stream.StreamResult
import java.io._

import Binding._

@XmlRootElement()
@XmlAccessorType(XmlAccessType.FIELD)
class Customer (
    val name:String, 
    val age:Int)
{
	private def this() = this("", 0)
}

@XmlRootElement()
@XmlAccessorType(XmlAccessType.FIELD)
class CustomerList(
    @XmlElement @XmlJavaTypeAdapter(classOf[ListAdapter]) val clist:List[Customer]){
  
  private def this() = this(List())
}

class ListAdapter extends AbstractListAdapter[Customer, JList[Customer]] {
}
  
object CustomerTest extends App {
  
  val cus = new Customer("tom", 28)
  val l = new CustomerList(List(cus))
  val context = JAXBContext.newInstance(classOf[CustomerList])
  val marshaller = context.createMarshaller();
  marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
  marshaller.marshal(l, System.out);

}