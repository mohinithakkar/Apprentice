package main
import sevenzip._
import java.io._
import java.util.Scanner
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._
object TApp extends App {

  val msg = "This is a secrect message from Georgia Tech."
  val seven = new SevenZip()
  val bytes = seven.encode(msg);
  println(bytes.mkString(" "))

  val pw = new FileOutputStream("7ztest.lzma")
  pw.write(bytes)
  pw.close
    val byteStream = new FileInputStream("7ztest.lzma")
    val length = byteStream.available()
    val b = new Array[Byte](length)
    byteStream.read(b)

  //  val input = new BufferedInputStream(new FileInputStream("7ztest.lzma"));
  //  val result = readAndClose(input);
//  Thread.sleep(2000)
//  println("reading")
//  val r = new ArrayBuffer[Byte]()
//  val scan = new Scanner("7ztest.lzma")
//  while (scan.hasNextByte()) {
//    r += scan.nextByte()
//  }
  println(b.mkString(" "))
  val back = seven.decode(b)
  println(back)
  println(seven.compressionRate())
}