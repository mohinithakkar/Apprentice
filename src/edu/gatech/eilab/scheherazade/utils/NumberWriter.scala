package edu.gatech.eilab.scheherazade.utils

import java.io._

/** Add numbers to the unnumbered stories (as produced by CSVProcessor)
 * 
 */
object NumberWriter extends App {

  val text = scala.io.Source.fromFile("./data/airport/airport stories.txt").mkString

  val writer = new PrintWriter(new BufferedOutputStream(new FileOutputStream("./data/airport/airportStories.txt")))

  val lines = text.split("\n")

  var i = 0
  lines foreach { line =>
    val text = line.trim
    if (text != "###") {
      writer.println(i + " " + text)
      i += 1
    }
    else writer.println("###")
  }

  writer.close

}