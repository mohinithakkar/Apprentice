package object data {

  import com.thoughtworks.xstream.converters._
  import com.thoughtworks.xstream.converters.collections._
  import com.thoughtworks.xstream._
  import com.thoughtworks.xstream.mapper._
  import com.thoughtworks.xstream.io._
  import com.thoughtworks.xstream.annotations._

  val XStream = new XStream()
  ListConverter.configureXStream(XStream)
}