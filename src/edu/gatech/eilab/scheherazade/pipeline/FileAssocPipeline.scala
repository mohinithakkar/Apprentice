package edu.gatech.eilab.scheherazade.pipeline
import java.io._
abstract class FileAssocPipeline[-I, +O] extends Pipeline[I, O] {
  var path: String
  var discardFile: Boolean

  def bufferName() =
    if (path.endsWith("/") || path.endsWith("\\")) path + cascadedName + ".buffer"
    else path + "/" + cascadedName + ".buffer"

  def writeToFile[O](output:O)
  def readFromFile(): O

  def fileExists() = {
    val file = new File(bufferName)
    file.exists()
  }

  def work(input: I): O

  /** read from the buffer file if it exists and is not forbidden with discardFile
   * otherwise, compute the output
   */
  def apply(input: I): O = {
    if (!discardFile && fileExists) {
      readFromFile()
    }
    else {
      val output = work(input)
      writeToFile(output)
      discardFile = false // the file has been updated and is now usable
      output
    }
  }
}
