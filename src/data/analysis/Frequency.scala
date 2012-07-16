package data.analysis
import main._
import data._

object Frequency extends App{
 val reader = new ConfigReader("configNewMv.txt")
 
 
 var (stories, clusters) = reader.initData()
 val used = clusters.flatMap(_.members)
 val unused = stories.flatMap(_.members).filterNot(sent => used.contains(sent))
 println(unused.map(_.toShortString()).mkString("\n"))
 
 
 /*
 var (stories, clusters) = reader.initDataFiltered()
 var list = clusters.map(c => (c.name, c.members.size))
 list = list.sortWith((c1, c2) => c1._2 > c2._2)
 println(list.map(x => x._1 + ", " + x._2).mkString("\n"))
 */
}