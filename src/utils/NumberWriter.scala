package utils

import java.io._

object NumberWriter extends App {

  val text = scala.io.Source.fromFile("./data/robbery/robberyStories Unnumbered.txt").mkString

  val writer = new PrintWriter(new BufferedOutputStream(new FileOutputStream("./data/robbery/robberyStories.txt")))

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